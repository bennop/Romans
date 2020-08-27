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
    expect_error(to_roman(  -1), '-1 cannot be expressed in Roman numerals')
    expect_error(to_roman(   0), '0 cannot be expressed in Roman numerals')
    expect_error(to_roman(4000), '4000 cannot be expressed in Roman numerals')
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

test_that("roman digit conversion works", {
    expect_equal(roman_digit_to_number("I"),    1)
    expect_equal(roman_digit_to_number("V"),    5)
    expect_equal(roman_digit_to_number("X"),   10)
    expect_equal(roman_digit_to_number("C"),  100)
    expect_equal(roman_digit_to_number("M"), 1000)
    # special cases
    # - multiple characters
    expect_equal(roman_digit_to_number("IX"),   1)
    # - lower case characters
    expect_equal(roman_digit_to_number("i"),    1)
})

test_that("roman digit conversion fail", {
    expect_error(roman_digit_to_number("B"), '"B" is not a valid Roman numeral')
    expect_error(roman_digit_to_number("z"), '"z" is not a valid Roman numeral')
    expect_error(roman_digit_to_number("-"), '"-" is not a valid Roman numeral')
})

test_that("roman digit conversion works", {
    expect_equal(roman_to_decimal("I"),             1)
    expect_equal(roman_to_decimal("i"),             1)
    expect_equal(roman_to_decimal("V"),             5)
    expect_equal(roman_to_decimal("X"),            10)
    expect_equal(roman_to_decimal("C"),           100)
    expect_equal(roman_to_decimal("M"),          1000)
    expect_equal(roman_to_decimal("II"),            2)
    expect_equal(roman_to_decimal("iv"),            4)
    expect_equal(roman_to_decimal("XCV"),          95)
    expect_equal(roman_to_decimal("MDCCCX"),     1810)
    expect_equal(roman_to_decimal("MMDCCCXCIX"), 2899)
    expect_equal(roman_to_decimal("MMMCMXCIX"),  3999)
    # failure
    expect_error(roman_to_decimal("MAM"), '"A" is not a valid Roman numeral')
})
