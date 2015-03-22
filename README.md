# Welcome to ESCAD.

ESCAD stands for **E**xpandable **S**ymbolic **C**omputer **A**ided **D**escription.
It is a tool which allows you to model parts of the real world in symbolic form with defined taxonomy.
Therefore it uses 2 objects:
 - Symbols
 - Relations.

Symbols represent things or processes in the real world. For example a "man" could be a symbol.
Relations represent relations between the symbols. For example "is_friend_of" could be a relation between to
"man"-symbols.

After modeling the desired topics, you can generate views, reports or other things.
The whole system is extendable through expansions, so you are not limited to the predefined general expansions.

ESCAD is usable through some user-interfaces or API.

Watch video about escad at [Youtoube](https://youtu.be/OTEUX44zbIQ) to get a feeling what is
possible with escad.

![Overview](../wiki/escad_overview.png)

# USAGE

## Command-line-client
  * Start the system in the escad directory with: ./escad_server start terminal
  * Execute "(help)" in escad to get the newest information about escad. There is also a built in tutorial, so you can
    interactively learn how to use escad.
  * Stop the system in the escad directory with: ./escad_server stop

## Web-interface-client
  * Start the server in the escad directory with: ./escad_server start net-rest
  * Open Web-Browser and type: http://127.0.0.1:4000
  * Stop the system in the escad directory with: ./escad_server stop

## Net-Interface-API (for programms to use)
  * Start the server in the escad directory with: ./escad_server start net-lisp
  * Open Web-Browser and type: http://127.0.0.1:4000
  * Stop the system in the escad directory with: ./escad_server stop

# COPYRIGHT

COPYRIGHT on ESCAD has Markus Kollmar. To read more about that in file "README" in repository.

HAVE FUN WITH ESCAD.

:-)
