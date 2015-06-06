# HydraBot

An IRC bot.


## Features

- Concurrency-friendly: separate reading and writing, multiple message
  processing and background processes.
- News (comics) scraping
- Slaps
- Knowledge base (in-memory for now)


## Installation

Lightyear is required. Tested on Idris 0.9.18.

```
idris -p contrib -p lightyear Main.idr -o bot
./bot <ipv4 address> <port> <nick> <user> <user info> [#channel1 #channel2...]
```


## Design

Nothing fancy: it's an attempt to quickly write a working program (and
maybe gradually improve it later). Currently there's even no proper
IRC message parsing.
