# check_table_arg respects custom table_id

    Code
      check_table_arg(quo("original"), comp_default)
    Condition
      Error:
      ! Problem with argument `table = "original"`
      i `table` must be either "a" or "b"

---

    Code
      check_table_arg(quo("a"), comp_custom)
    Condition
      Error:
      ! Problem with argument `table = "a"`
      i `table` must be either "original" or "updated"

---

    Code
      check_table_arg(quo("b"), comp_custom)
    Condition
      Error:
      ! Problem with argument `table = "b"`
      i `table` must be either "original" or "updated"

---

    Code
      check_table_arg(quo(c("a", "b")), comp_default)
    Condition
      Error:
      ! Problem with argument `table = c("a", "b")`
      i `table` must be a single character value: "a" or "b"

---

    Code
      check_table_arg(quo(), comp_default)
    Condition
      Error:
      ! `table` is absent but must be supplied.

