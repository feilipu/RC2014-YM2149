# RC2014-YM2149

My patched PTX Player that will read a PT3 file and play it, press a key to exit...

I have included assembled binary configured for the YM2149 at D0 hex ([Revision 5](https://z80kits.com/shop/ym2149-sound-card/)).

A binary can be built for the [Revision 6 Module](https://github.com/electrified/rc2014-ym2149), by setting the `REV5` option equal to 0.
This will enable the MSX addressing available on Revision 6 Modules.

To assemble, you need to use `z88dk-z80asm`.

```sh
z88dk-z80asm  PS-PTxPlay.asm -m -l -b
```

`-m` generates a map file. `-l` generates a list file. These are both optional.

The resulting `PS-PTxPlay_code_user.bin` file can be renamed as desired (eg `ptxplay.com`) and uploaded to your RC2014 to be used with any CP/M implementation.

The z88dk can be found at [Z88DK](https://github.com/z88dk/z88dk).
