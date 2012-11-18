#!/bin/bash
sudo sendip -p ipv4 -p udp -us 5070 -ud 29829 -d "123@Hello 1 123@provet 2" -v 127.0.0.1
