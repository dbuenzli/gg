# Tests

A few tests need `vg` and `brr`: 

```
opam install vg brr
```

Or `b0 file gather` them with this repo.

# Test and debug Pgon2 boolean ops

We need `vg`, but the latter depends on `gg`, so unless `vg` is part
of the build we need to exclude `gg` when we build otherwise we get
inconsistent assumptions. Either:

```
b0 -x gg -a pgon2_bool_tests
b0 -x gg -a pgon2_bool_steps
```

or the `pgon_bool_tests` is a locked pack which has the tests but not `gg`:

```
b0 -p pgon2_bool_tests -a …
```




