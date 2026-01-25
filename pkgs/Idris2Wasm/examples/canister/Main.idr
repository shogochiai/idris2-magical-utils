module Main

-- Simple Idris2 canister example
-- The actual canister entry points are in support/ic0/canister_entry.c
-- This Idris2 code initializes the runtime

greet : String
greet = "Hello from Idris2!"

main : IO ()
main = pure ()
