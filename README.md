# LLTZ request for comments

This is a draft implementation of the new shared backend for all Tezos
languages. This is a very early stage draft, no hard decisions have been made,
this is a starting point to iterate on so feel free to suggest any changes.

## Contributing

- Raise issues with any thoughts or suggested changes.
- Feel free to raise draft PR's and comment on other PR's. If there is
  sufficient agreement convert it out of draft mode.
- The aim is to create an initial implementation with as much concensus as
  possible, with the understanding that whatever we come up with will evolve
  during the implementation.

## Next Steps

- Decide how we will share the code between languages. (Git submodules opam
  packages etc.)
- Create passes from Michel and Mini-c to LLTZ IR
- Compile LLTZ IR to Michelson.
