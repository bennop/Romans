test_that("digits work", {
    expect_equal(to_roman(   1), 'I')
    expect_equal(to_roman(   5), 'V')
    expect_equal(to_roman(  10), 'X')
    expect_equal(to_roman(  50), 'L')
    expect_equal(to_roman( 100), 'C')
    expect_equal(to_roman( 500), 'D')
    expect_equal(to_roman(1000), 'M')
})

test_that("compounds work", {
    expect_equal(to_roman(   2), 'II')
    expect_equal(to_roman(   4), 'IV')
    expect_equal(to_roman(   8), 'VIII')
    expect_equal(to_roman(   9), 'IX')
    expect_equal(to_roman(  35), 'XXXV')
    expect_equal(to_roman( 999), 'CMXCIX')
    expect_equal(to_roman(3888), 'MMMDCCCLXXXVIII')
})

test_that("special cases work", {
    expect_equal(to_roman(   pi), 'III')
})

test_that("impossible numbers fail", {
    expect_error(to_roman(  -1), '-1 cannot be expressed in roman numerals')
    expect_error(to_roman(   0), '0 cannot be expressed in roman numerals')
    expect_error(to_roman(4000), '4000 cannot be expressed in roman numerals')
})


test_that("Roman range works", {
    expect_equal(in_roman_range(   0), FALSE)
    expect_equal(in_roman_range(   1), TRUE)
    expect_equal(in_roman_range(3999), TRUE)
    expect_equal(in_roman_range(4000), FALSE)
})

test_that("lookup works", {
    expect_known_hash(ROMANS, hash = 'd8da97194b')
})

test_that("digit extraction works", {
    expect_equal(num_to_digits(  -1), numeric(0))
    expect_equal(num_to_digits(   0), numeric(0))
    expect_equal(num_to_digits(   1), 1)
    expect_equal(num_to_digits( 123), 1:3)
    expect_equal(num_to_digits(3889), c(3,8,8,9))
})
