# RC2014-YM2149

My patched PTX Player that will read a PT3 file and play it, press a key to exit...

I have included assembled binary. Requires that YM2149 at D0 hex.

To assemble, you need to use `z88dk-z80asm`.

```sh
z88dk-z80asm  PS-PTxPlay.asm -m -l -b
```


z88dk can be found at [Z88DK](https://github.com/z88dk/z88dk).
