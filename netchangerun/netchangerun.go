package main

// int poll_netlink(int tout_ms);
import "C"
import (
	"flag"
	"fmt"
	"net"
	"net/http"
	"os"
	"strings"
	"time"

	"github.com/coreos/go-log/log"
	"github.com/coreos/go-systemd/daemon"
)

type PriorityFilter struct {
	priority log.Priority
	target   log.Sink
}

var (
	iface = flag.String("iface", "", "Interface to watch.")
	url   = flag.String("url", "", "URL to get when we see a new address, "+
		"{{}} will be replaced by the new address.")
	isIPv6  = flag.Bool("ipv6", false, "Look for IPv6 address.")
	verbose = flag.Bool("verbose", false, "Verbose logging.")
	current string
	l       = log.NewSimple(
		&PriorityFilter{
			log.PriInfo,
			log.WriterSink(os.Stdout, log.BasicFormat, log.BasicFields),
		})
)

func (filter *PriorityFilter) Log(fields log.Fields) {
	if *verbose || fields["priority"].(log.Priority) <= filter.priority {
		filter.target.Log(fields)
	}
}

func wantedType(addr string) bool {
	ip := net.ParseIP(addr)
	if ip == nil || !ip.IsGlobalUnicast() {
		return false
	}
	if ip.To4() == nil && *isIPv6 {
		return true
	}
	return !*isIPv6
}

func run(addr string) {
	daemon.SdNotify(false, "WATCHDOG=1")
	u := strings.Replace(*url, "{{}}", addr, -1)
	l.Debugf("GET %s", u)
	resp, err := http.Get(u)
	if err != nil {
		l.Errorf("GET failed: %v", err)
		current = ""
		return
	}
	defer resp.Body.Close()
	if resp.StatusCode != 200 {
		l.Errorf("GET retuned 200: %s", resp.Status)
		current = ""
		return
	}
}

func stripNetmask(addr string) string {
	parts := strings.Split(addr, "/")
	return parts[0]
}

func getNewCurrent() error {
	if current != "" {
		return nil
	}
	ifaces, err := net.Interfaces()
	if err != nil {
		return fmt.Errorf("Cannot get list of interfaces: %v", err)
	}
	for _, i := range ifaces {
		if i.Name == *iface && (i.Flags&net.FlagUp) != 0 {
			addrs, err := i.Addrs()
			if err != nil {
				return fmt.Errorf("Cannot get addresses for interface %s: %v", i.Name, err)
			}
			for _, a := range addrs {
				addr := stripNetmask(a.String())
				if wantedType(addr) {
					current = addr
					l.Infof("New address %s for %s", current, *iface)
					run(current)
					return nil
				}
			}
		}
	}
	return nil
}

func checkCurrent() error {
	ifaces, err := net.Interfaces()
	if err != nil {
		return fmt.Errorf("Cannot get list of interfaces: %v", err)
	}
	for _, i := range ifaces {
		if i.Name == *iface && (i.Flags&net.FlagUp) != 0 {
			addrs, err := i.Addrs()
			if err != nil {
				return fmt.Errorf("Cannot get addresses for interface %s: %v", i.Name, err)
			}
			for _, a := range addrs {
				addr := stripNetmask(a.String())
				if addr == current {
					return nil
				}
			}
		}
	}
	current = ""
	return nil
}

func ifaceName(ifIndex int) string {
	if iface, _ := net.InterfaceByIndex(ifIndex); iface != nil {
		return iface.Name
	}
	return fmt.Sprintf("[if_index:%d]", ifIndex)
}

func renew() {
	if err := checkCurrent(); err != nil {
		l.Error(err)
		os.Exit(1)
	}
	if err := getNewCurrent(); err != nil {
		l.Error(err)
		os.Exit(1)
	}
}

//export NewAddress
func NewAddress(ifIndex int, addr_ *C.char) {
	addr := C.GoString(addr_)
	l.Debugf("NewAddress %s %s", ifaceName(ifIndex), addr)
	if current != "" {
		return
	}
	renew()
}

//export DelAddress
func DelAddress(ifIndex int, addr_ *C.char) {
	addr := C.GoString(addr_)
	l.Debugf("DelAddress %s %s", ifaceName(ifIndex), addr)
	renew()
}

//export NewLink
func NewLink(ifIndex int) {
	l.Debugf("NewLink %s", ifaceName(ifIndex))
	renew()
}

//export DelLink
func DelLink(ifIndex int) {
	l.Debugf("DelLink %s", ifaceName(ifIndex))
	renew()
}

func main() {
	flag.Parse()
	if *iface == "" || *url == "" {
		l.Error("Both iface and url parameters are mandatory.")
		os.Exit(1)
	}
	if err := getNewCurrent(); err != nil {
		l.Error(err)
		os.Exit(1)
	}
	interval, err := daemon.SdWatchdogEnabled(false)
	if err != nil || interval == 0 {
		interval = 5 * time.Second
	}
	daemon.SdNotify(false, "READY=1")
	for {
		if r := C.poll_netlink(C.int(interval.Nanoseconds() / 1000000)); r != 0 {
			l.Errorf("Error: %d", r)
			os.Exit(1)
		}
		daemon.SdNotify(false, "WATCHDOG=1")
		renew()
	}
}
