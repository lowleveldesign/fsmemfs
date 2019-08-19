
# fsmemfs

Fsmemfs is a simple memory file system based on [WinFsp](https://github.com/billziss-gh/winfsp) (WinFsp - Windows File System Proxy, Copyright (C) Bill Zissimopoulos). I wrote it to test various options of the WinFsp and also to learn F#. It works but it's not fully tested so please be warned.

The available options are:

```
Usage: fsmemfs OPTIONS

Options:
  -m VALUE     [required] mount point, could be a drive (G:) or a folder (C:\memfs)
  -v           [optional] enable verbose log
  -l VALUE     [optional] a path to a file where fsmemfs should write logs
  -D VALUE     [optional] a path to a file where WinFsp should write debug logs
```

To create a new G: volume, simply run: `fsmemfs -m G:`.

Additionally, to see some logs from the kernel driver (-D) and user-mode driver (-l), run: `fsmemfs -v -l d:\temp\fsmemfs.log -D d:\temp\fsmemfs-driver.log -m G:`.
