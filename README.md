# Haskell LibP2P - MultiStream implementation
WIP

# todo:
 - start using quick check to implement tests for handling of multistream muxer.
 - need to guarantee multistream protocols start with a '/' and have the correct format (similar to multiaddr)
 - need to implement handling of behaviour for parser exceptions inline with go-multistream
 - need to include a transport shim for haskell multistream based on go-multistream-muxer
 - A lot of the functions over MultistreamMessage are not total functions. Not sure how to deal with this at the moment, other than using a phantom type that splits either Multistream or MultistreamMessage by header or body?

# notes:
 - When running multistream in ghci using stack, 
   use `stack ghci --ghci-options -XOverloadedStrings` to pull in the language extension

# test suite:
 - run `stack test`
