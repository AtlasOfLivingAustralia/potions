# potionstest

This is a micro-package stored inside `potions` for test purposes. It exists
because `potions::brew()` includes functionality to detect whether it is
being called within another package, and if so, to store data in a dedicated
`"packages"` slot. Testing this functionality requires a test package, hence
`{potionstest}`.

Obviously, this package isn't built to do anything that is useful outside of 
this very narrow use case.