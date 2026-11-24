# spl3g's server configs

## Folder structure
```
.
├── README.md
├── flake.lock
├── flake.nix
├── hosts
│   ├── ltrr-block
│   ├── ltrr-cloud # vps
│   └── ltrr-pad # my laptop home server
├── modules # reusable modules
│   ├── default.nix
│   ├── services
│   └── system
├── overlays # stable/unstable package overlays
├── profiles # general configurations
│   ├── cloud
│   └── home
└── secrets # some encrypted wireguard secrets
    ├── cloud
    └── home
```
