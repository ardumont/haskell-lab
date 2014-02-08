Wifi.hs
=======

A simple wifi module haskell to help in connecting to auto-connect Wifi.

# What

This will:
- Scan the current available wifis
- Compute your autoconnect list of wifis
- Elect the one with the most powerful signal and try to connect to it

# How

Embedding this in a script:

```sh
runhaskell Wifi.hs
```

# Pre-requisite

## Packages

- haskell-platform
- nmcli (which is packaged with NetworkManager tools in debian-based machine).

## Wifi setup

You already have some connection settings applied to your machine.

# platform

Tested only on debian-based machine

# Tested use case

## no concurrency

- Disconnect your wifi
- Run the command
- This will connect you 

## concurrency

- Let your current wifi connected
- Activate the tethering on your mobile phone and let your mobile phone near your computer.
- Run the command
- This will connect you over the tethering connection which must be more powerful than your other wifi connections
