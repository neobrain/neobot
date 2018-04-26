# neobot

A cool IRC bot - written in Haskell!

## Building

`stack setup && stack build && stack exec neobot`

## Configuration

neobot loads its configuration from a file called `config.json` on startup. The file is structured like this:

```
{
    "networks" : [{
        "name" : "freenode",
        "server" : "chat.freenode.net",
        "port" : 6667,
        "nick" : "neobo7",
        "channels" : [{
            "name" : "#neobot",
            "offensive_language" : false
        }]
    }],
    "auth_token" : "<GitHub auth token>",
    "kpop_videos" : []
{
```
