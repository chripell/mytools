# Using the Dokicam with Linux

This is a collection of notes about using the
[Dokicam](http://www.dokicam.com) with Linux (or on other platforms of
course). The information was gathered using tcpdump on Android. The
application is pretty good, but I want to use the little roundly camera
from my Linux desktop as well and I am a bit pissed off by the many
permissions the app asks (and also some call home connection from it
don't make me super-happy). Just to be clear: the reverse engineering
was made for the sole purpose of interoperability with Linux.

## Connecting to the Dokicam

The little ball runs an AP with WPA and a DHCP server. It is a bit
annoying because if you are connected to the Interweb via WiFi. I
would suggest to use an USB WiFI adapter. Supposing the name of the
device is *wlp0s29f7u7u3u1*, you just need to:

```shell
ip link set wlp0s29f7u7u3u1 up
wpa_supplicant -i wlp0s29f7u7u3u1 -c <(wpa_passphrase "DOKICAM-D1H3P0424" "POODLE")
```

where *POODLE* is the password you set in the Dokicam app (have you
changed from the default one, haven't you?) and *DOKICAM-D1H3P0424* is
th network SSID you can see int the Android network list. Usually the
dokicam has the IP address *192.168.1.254*.

## Downloading, shooting images or videos, setting parameters

I wrote a small Go application for this purpose. Run it without
arguments for the help screen. The dokicam has a webserver on port 80
with a simple, somehow obscured or just badly designed, XML over HTTP
protocol. There is also port 3333 which continuously sends the battery
level and port 443, which I haven't investigated. For example, if you
want to download all the images from the dokicam to the current
directory:

```shell
dokicam photo
for i in $(dokicam list); do dokicam get $i; done
```

## Previewing in photo mode

The dokicam sends a stream of jpegs on port 8192 which can be easily
seen with:

```shell
mplayer -demuxer lavf http://192.168.1.254:8192/
```
## Previewing in video mode

You can check the RTSP stream with:

```shell
mplayer rtsp://192.168.1.254:554/XXX.mov
```

## Viewing via UVC

The black ball is recognized by *libinput* (after you long-press the
shutter button as per manual; otherwise it is in USB mass storage
mode). I tested on Arch Linux with kernel 4.11.9. You can use
*guvcview* to dispay the output from the device *DOKI*.

## Converting from 2 fisheye images to an equiangular panorama

For now I use the Windows application from their
[site](http://dokicam.com/downloads/0) under Wine32. I know, shame,
shame, shame. It shouldn't be difficult to use *Hugin* to do the
remapping, I just haven't had time to look into it yet.


