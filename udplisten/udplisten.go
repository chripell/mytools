package main

import (
	"flag"
	"fmt"
	"net"
)

func main() {
	var port = flag.Int("port", 512, "Port number where to listen.")
	addr, err := net.ResolveUDPAddr("udp4", fmt.Sprintf("0.0.0.0:%d", *port))
	if err != nil {
		panic(err)
	}
	conn, err := net.ListenUDP("udp", addr)
	if err != nil {
		panic(err)
	}
	defer conn.Close()
	for {
		buf := make([]byte, 1500)
		_, addr, err := conn.ReadFromUDP(buf)
		if err != nil {
			panic(err)
		}
		fmt.Printf("%s: %s\n", addr, buf)
	}
}
