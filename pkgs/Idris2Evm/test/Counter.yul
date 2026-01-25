object "Counter" {
  code {
    datacopy(0, dataoffset("runtime"), datasize("runtime"))
    return(0, datasize("runtime"))
  }
  object "runtime" {
    code {
      // Dispatcher
      switch selector()
      case 0x371303c0 /* increment() */ {
        increment()
      }
      case 0xb3bcfa82 /* decrement() */ {
        decrement()
      }
      case 0x06661abd /* count() */ {
        let c := sload(0)
        mstore(0, c)
        return(0, 32)
      }
      default {
        revert(0, 0)
      }

      function selector() -> s {
        s := div(calldataload(0), 0x100000000000000000000000000000000000000000000000000000000)
      }

      function increment() {
        let c := sload(0)
        sstore(0, add(c, 1))
      }

      function decrement() {
        let c := sload(0)
        if gt(c, 0) {
          sstore(0, sub(c, 1))
        }
      }
    }
  }
}
