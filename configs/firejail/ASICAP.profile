mkdir ~/capture
whitelist ~/capture
whitelist ~/ASICAP
whitelist ~/.config/ASICAP

protocol unix,netlink

include /etc/firejail/whitelist-common.inc
include /etc/firejail/default.profile


