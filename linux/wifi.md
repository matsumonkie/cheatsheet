# wifi troubleshooter

## Do I have a network device?

run `lspci` to list all PCI devices (devices connected to your motherboard). If your network card is embedded (otherwise use `lsusb` if connected through usb port). Check for `Network`, `wireless`, `WLAN`, `wifi`, or `802.11`.
e.g: `lspci | grep Network` returns my wifi card e.g: `6c:00.0 Network controller: Qualcomm Device 1101 (rev 01)`

list hardware `sudo lshw` (grep for `network` or used `sudo lshw -C network`) will show us that driver used is `ath11k_pci`
```
...
           *-network
                description: Network controller
                product: Qualcomm
                vendor: Qualcomm
                physical id: 0
                bus info: pci@0000:6c:00.0
                version: 01
                width: 64 bits
                clock: 33MHz
                capabilities: pm msi pciexpress bus_master cap_list
                configuration: driver=ath11k_pci latency=0
                resources: irq:170 memory:b4200000-b42fffff
...
```

## driver status

you can check the driver status by running `lsmod` which shows the status of the modules in the linux kernel.
```
> sudo lsmod | grep ath11k
ath11k_pci             24576  0
ath11k                405504  1 ath11k_pci
qmi_helpers            32768  1 ath11k
```

0 means it's not used

## enable a driver

run `sudo modprobe modulename` to enable the module.


## show wireless network interface

run `iwconfig`
