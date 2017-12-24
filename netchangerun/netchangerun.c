
#include "_cgo_export.h"

#define _GNU_SOURCE

#include <string.h>
#include <poll.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>

#include <arpa/inet.h>
#include <ifaddrs.h>
#include <linux/if.h>
#include <linux/netlink.h>
#include <linux/rtnetlink.h>
#undef __USE_MISC
#include <net/if.h>
#include <netinet/in.h>
#include <sys/socket.h>

static int sock = -1;

static void msg_handler (struct sockaddr_nl *nl, struct nlmsghdr *msg) {
  struct ifaddrmsg *ifa;
  struct ifinfomsg *ifi;
  struct rtattr *rth;
  int rtl;
  char buf[INET6_ADDRSTRLEN + 1];

  ifa = (struct ifaddrmsg *) NLMSG_DATA (msg);
  ifi = (struct ifinfomsg *) NLMSG_DATA (msg);
  switch (msg->nlmsg_type) {
  case RTM_NEWADDR:
  case RTM_DELADDR:
    rth = IFA_RTA (ifa);
    rtl = IFA_PAYLOAD (msg);
    while (rtl && RTA_OK (rth, rtl)) {
      if ((rth->rta_type == IFA_LOCAL /* IPv4 */
	   || rth->rta_type == IFA_ADDRESS /* IPv6 */)
	  && ifa->ifa_scope == RT_SCOPE_UNIVERSE /* no IPv6 link */) {
	inet_ntop(ifa->ifa_family, RTA_DATA (rth), buf, sizeof(buf));
	if (msg->nlmsg_type == RTM_NEWADDR)
	  NewAddress(ifi->ifi_index, buf);
	else
	  DelAddress(ifi->ifi_index, buf);
      }
      rth = RTA_NEXT (rth, rtl);
    }
    break;
  case RTM_NEWLINK:
  case RTM_DELLINK:
    if (msg->nlmsg_type == RTM_NEWLINK)
      NewLink(ifi->ifi_index);
    else 
      DelLink(ifi->ifi_index);
    break;
  }
}

int poll_netlink(int tout_ms) {
  int ret;
  char buf[4096];
  struct iovec iov = {buf, sizeof buf};
  struct sockaddr_nl snl;
  struct msghdr msg = {&snl, sizeof(snl), &iov, 1, NULL, 0, 0};
  struct nlmsghdr *h;
  struct pollfd pfd[1];
	
  if (sock < 0) {
    	struct sockaddr_nl addr;
	
	memset (&addr, 0, sizeof(addr));
	sock = socket (AF_NETLINK, SOCK_RAW, NETLINK_ROUTE);
	if (sock < 0)
	  return errno;
	addr.nl_family = AF_NETLINK;
	addr.nl_pid = getpid();
	addr.nl_groups = RTMGRP_LINK | RTMGRP_IPV4_IFADDR | RTMGRP_IPV6_IFADDR;
	if (bind(sock, (struct sockaddr *) &addr, sizeof(addr)) < 0) {
	  sock = -1;
	  return errno;
	}
  }
  if (tout_ms > 0) {
    pfd[0].fd = sock;
    pfd[0].events = POLLIN;
    switch (poll(pfd, 1, tout_ms)) {
    case -1:
      return errno;
    case 0:
      return 0;
    }
  }
  ret = recvmsg (sock, &msg, 0);
  if (ret < 0) {
    if (errno == EWOULDBLOCK || errno == EAGAIN || errno == EINTR)
      return 0;
    return errno;
  }
  if (ret == 0) {
    close(sock);
    sock = -1;
    return 0;
  }
  for (h = (struct nlmsghdr *) buf; NLMSG_OK(h, ret); h = NLMSG_NEXT (h, ret)) {
    if (h->nlmsg_type == NLMSG_DONE)
      return 0;
    if (h->nlmsg_type == NLMSG_ERROR) {
      return 10000;
    }
    msg_handler(&snl, h);
  }
  return 0;
}
