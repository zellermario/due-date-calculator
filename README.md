# Due date calculator

This project can be built and be tested with [Stack](https://docs.haskellstack.org/en/stable/README/).

Build the project using the `stack build` command, or build and run the tests with `stack test`.

## Some notes

- Except for the `calculateDueDate` entrypoint, all the functions are pure, so they were easy to be tested.
- The properties of the working week can be modified in the `src/BusinessWeek.hs` file, but now it is set to be Monday-Friday 9:00-17:00, and the test cases also expect these settings.
- QuickCheck is used for some test cases, which generate random parameters for the test properties using the type's `Arbitrary` instance. The `Arbitrary` instance for `LocalTime` was not directly available from the `time` package, so I copied it from the library's test files.
- Some tests had to be modified subsequently for two reasons:
  1. The implementation is only accurate to minutes so the time equality testings had to be loosened.
  2. Even though the entrypoint function rejects all submissions out of business hours, and does not forward them to calculation, the test cases might provide out-of-business-hour inputs to the tested pure functions, causing them to fail. This is resolved by only ever failing the affected test case runs if the input was valid. (A better solution would be to modify the relevant `Arbitrary` instance to only generate valid inputs, but I had no time for that).