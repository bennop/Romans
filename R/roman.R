

ROMANS <- matrix(c('I', 'V', 'X', 'L', 'C', 'D', 'M', NA),
                 nrow = 2)

#' Convert number to roman numeral
#'
#' It is assumed that \code{number} is an integer value
#' @param number integer (any fractional part is truncated)
#'
#' @return roman numeral corresponding to \code{number} (or error when out of range)
#' @export
#'
#' @examples
#' to_roman(1964)
to_roman <- function(number){
    if(!in_roman_range(number)){
        stop(number, " cannot be expressed in roman numerals")
    }
    digits <- num_to_digits(floor(number))
    roman <- ''

    ld <- length(digits)
    for(i in 1:ld){
        di <- digits[i]
        if(di %in% c(4,9)){
            new_roman <- paste0(ROMANS[                   1 , ld-i+1],
                                ROMANS[ifelse(di == 9, 1, 2), ld-i+1+(di == 9)])
        } else {
            new_roman <- ''
            if(di > 4){
                new_roman <- ROMANS[2 , ld-i+1]
                di <- di - 5
            }
            while(di > 0){
                new_roman <- paste0(new_roman, ROMANS[1 , ld-i+1])
                di <- di - 1
            }
        }
        roman <- paste0(roman, new_roman)
    }

    return(roman)
}

#' check whether number is in range for roman numerals
#'
#' @param number integer (not checked)
#'
#' @return boolean indicating possible conversion
#' @export
#'
#' @examples
#' in_roman_range(12)
in_roman_range <- function(number){
    return (number >= 1 && number < 4000)
}

#' convert integer to vector of digits
#'
#' @param number integer (not checked)
#'
#' @return
#' @export
#'
#' @examples
#' num_to_digits(123)
num_to_digits <- function(number){
    digits <- numeric(0)
    while(number>0){
        last_digit <- number %% 10
        digits <- c(last_digit, digits)
        number <- number %/% 10
    }
    return(digits)
}
