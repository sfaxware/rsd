In order to compile:
```sh
$ fpcmake # Generate make file
$ make # Build
```

In order to build Debian packages:
```sh
$ git clean -df
$ ./debian/rules pack-source
$ dpkg-buildpackage
```
