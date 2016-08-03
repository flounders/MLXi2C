# MLXi2c

This is just some sample code for using the HPi module with the Melexis MLX90614 contact-less
temperature sensor. You will need the [C library for BCM 2835](http://www.airspayce.com/mikem/bcm2835/)
to build the code.

To build this sample run the following:

```
$ cabal sandbox init
$ cabal build
# ./dist/build/MLXi2C/MLXi2C
```

There may be a way to run the program without root, but I haven't found it yet.
