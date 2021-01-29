#!/usr/bin/env python

import asyncio
import sys
import struct
import time
from dbus_next import BusType
from dbus_next.aio import MessageBus
from collections import deque


def get_signed_short(b, i):
    return struct.unpack("<h", b[i: i+2])[0]


def get_unsigned_short(b, i):
    return struct.unpack("<H", b[i: i+2])[0]


def get_unsigned_long(b, i):
    return struct.unpack("<L", b[i: i+4])[0]


async def new_adv(prop):
    for key, val in prop.items():
        data = val.value
        # print(time.time(), key, [hex(i) for i in data])
        print("MAC", ":".join(f"{i:02x}" for i in data[7:1:-1]))
        if len(data) == 18:
            print("Battery", get_unsigned_short(data, 8))
            print("Current Temperature", get_signed_short(data, 10) / 16.0)
            if key in (16, 17, 18):
                print("Current Humidity", get_signed_short(data, 12) / 16.0)
                print("Timestamp", get_unsigned_long(data, 14))
            else:
                print("Timestamp", get_unsigned_long(data, 12))
        elif len(data) == 20:
            print("Max Temperature", get_signed_short(data, 8) / 16.0)
            print("Max Temperature Timestamp", get_unsigned_long(data, 10))
            print("Min Temperature", get_signed_short(data, 14) / 16.0)
            print("Min Temperature Timestamp", get_unsigned_long(data, 16))
        else:
            print("Unhanded length:", len(data))
        print("----")


async def get_sb(bus, hci_name: str, mac_addr: str):
    sb_name = (hci_name + "/dev_" + mac_addr.replace(':', '_').upper())
    introspection = await bus.introspect('org.bluez', sb_name)
    return bus.get_proxy_object('org.bluez', sb_name, introspection)


async def scan(duration: int, hci_name: str, mac_addr: str):
    bus = await MessageBus(bus_type=BusType.SYSTEM).connect()
    hci_name = "/org/bluez/" + hci_name
    introspection = await bus.introspect('org.bluez', hci_name)
    dev_obj = bus.get_proxy_object('org.bluez', hci_name, introspection)
    dev = dev_obj.get_interface('org.bluez.Adapter1')
    sb_obj = await get_sb(bus, hci_name, mac_addr)
    sb = sb_obj.get_interface('org.freedesktop.DBus.Properties')
    sb.on_properties_changed(
        lambda iface, props, inv: asyncio.create_task(
            new_adv(props['ManufacturerData'].value))
        if 'ManufacturerData' in props else None)
    await dev.call_start_discovery()
    await asyncio.sleep(duration)
    await dev.call_stop_discovery()


class GattChar:

    def __init__(self, bus, name):
        if name is None:
            raise ValueError("Name is None")
        self.name = name
        self.bus = bus
        self.data = deque()
        self.data_lock = asyncio.Lock()
        self.data_cond = asyncio.Condition(lock=self.data_lock)

    async def connect(self, notify):
        introspection = await self.bus.introspect('org.bluez', self.name)
        self.cx_obj = self.bus.get_proxy_object(
            'org.bluez', self.name, introspection)
        self.cx = self.cx_obj.get_interface(
            'org.bluez.GattCharacteristic1')
        self.cx_prop = self.cx_obj.get_interface(
            'org.freedesktop.DBus.Properties')
        self.cx_prop.on_properties_changed(
            lambda iface, props, inv: asyncio.create_task(
                self._new_data(props['Value'].value))
            if 'Value' in props else None)
        if notify:
            await self.cx.call_start_notify()

    async def write(self, data: bytes):
        await self.cx.call_write_value(data, {})

    async def get_data(self) -> bytes:
        async with self.data_cond:
            await self.data_cond.wait_for(self._has_data)
            return self.data.popleft()

    def _has_data(self) -> bool:
        return len(self.data) > 0

    async def has_data(self) -> bool:
        async with self.data_lock:
            return self._has_data()

    async def _new_data(self, data):
        # print("DELME", self.name, [hex(i) for i in data])
        async with self.data_lock:
            self.data.append(data)
            self.data_cond.notify_all()

    async def flush(self):
        async with self.data_lock:
            self.data.clear()

    async def read(self) -> bytes:
        return await self.cx.call_read_value({})


class BTInspect:

    def __init__(self, bus):
        self.bus = bus

    async def connect(self):
        introspection = await self.bus.introspect('org.bluez', '/')
        self.root_obj = self.bus.get_proxy_object(
            'org.bluez', '/', introspection)
        self.object_manager = self.root_obj.get_interface(
            'org.freedesktop.DBus.ObjectManager')
        self.all = await self.object_manager.call_get_managed_objects()

    def gatt_by_uuid(self, uuid: str):
        for k, v in self.all.items():
            if 'org.bluez.GattCharacteristic1' in v:
                dev = v['org.bluez.GattCharacteristic1']
                if 'UUID' not in dev:
                    continue
                if uuid == dev['UUID'].value:
                    return k


async def extract(bus, hci_name: str, mac_addr: str):
    manager = BTInspect(bus)
    await manager.connect()
    fff3 = GattChar(
        bus, manager.gatt_by_uuid("0000fff3-0000-1000-8000-00805f9b34fb"))
    await fff3.connect(True)
    fff5 = GattChar(
        bus, manager.gatt_by_uuid("0000fff5-0000-1000-8000-00805f9b34fb"))
    await fff5.connect(False)
    await fff5.write(bytes([1, 0, 0, 0, 0]))
    data = await fff3.get_data()
    if data[0] != 1:
        print("Unexpected value on fff3", [hex(i) for i in data])
        return
    count_all = get_unsigned_long(data, 1)
    print("Samples, from older to newer, every 10 minutes:", count_all)
    cur = 0
    while cur < count_all:
        await fff5.write(
            bytes([7] + [(cur >> (8 * i)) & 0xff for i in range(4)]))
        data = await fff3.get_data()
        if data[0] != 7:
            print("Unexpected1 value on fff3", [hex(i) for i in data])
            return
        n = data[5]
        for i in range(n):
            print("T/H", get_signed_short(data, 6 + 2 * i) / 16.0,
                  get_signed_short(data, 6 + 2 * n + 2 * i) / 16.0)
        cur += n


async def dump(hci_name: str, mac_addr: str):
    hci_name = "/org/bluez/" + hci_name
    bus = await MessageBus(bus_type=BusType.SYSTEM).connect()
    sb_obj = await get_sb(bus, hci_name, mac_addr)
    sb = sb_obj.get_interface('org.bluez.Device1')
    await sb.call_connect()
    await extract(bus, hci_name, mac_addr)
    await sb.call_disconnect()


if len(sys.argv) < 4:
    print(f'Usage:\n {sys.argv[0]} scan [duration sec] [hci device] [mac address 70:F1:00:00:0F:7B]')
    print(f' {sys.argv[0]} dump [hci device] [mac address 70:F1:00:00:0F:7B]')
    sys.exit(1)
loop = asyncio.get_event_loop()
if sys.argv[1] == 'scan':
    loop.run_until_complete(scan(int(sys.argv[2]), sys.argv[3], sys.argv[4]))
elif sys.argv[1] == 'dump':
    loop.run_until_complete(dump(sys.argv[2], sys.argv[3]))
