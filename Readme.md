# Pack.re - a simple js bundler for reason

Pack javascript files together.
No frills.

Definitely alpha quality at the moment. But it works :D so far

```txt
# pack.re - a simple js bundler for reason

Usage: pack.re [options] entry-file.js > bundle.js

  --base (default: current directory)
      expected to contain node_modules
  --rename newName=realName (can be defined multiple times)
      maps `require("newName")` to a node_module called "realName"
  -h, --help
      print this help
```

## Global usage, if you want

```sh
npm i -g pack.re
pack.re --help
```

## Local usage

```sh
npm i pack.re
```

then add something to your package.json scripts, like "pack.re lib/js/src/main.js"

## License

MIT