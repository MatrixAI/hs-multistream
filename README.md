# Haskell LibP2P - MultiStream implementation
WIP
# todo:
 - need to write a parser for inputstream bytestring that:
    - Parses multicodecs, commands into some sort of sum type
 - start using quick check to implement tests for handling of multistream muxer 

# notes:
 - When running multistream in ghci using stack, 
   use `stack ghci --ghci-options -XOverloadedStrings` to pull in the language extension
