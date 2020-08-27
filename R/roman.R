

ROMANS <- matrix(c('I', 'V', 'X', 'L', 'C', 'D', 'M', NA),
                 nrow = 2)

#' Convert number to Roman numeral
#'
#' It is assumed that \code{number} is an integer value
#' @param number integer (any fractional part is truncated)
#'
#' @return Roman numeral corresponding to \code{number} (or error when out of range)
#' @export
#'
#' @examples
#' to_roman(1964)
to_roman <- function(number){
    if(!in_roman_range(number)){
        stop(number, " cannot be expressed in Roman numerals")
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

#' check whether number is in range for Roman numerals
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
#' @return digit vector
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


#' convert single-character Roman numeral to number
#'
#' If \code{rd} is not just a single character, only the first character is
#' considered.
#'
#' @param rd single character
#'
#' @return decimal value corresponding to Roman number \code{rd} (or error)
#' @export
#'
#' @examples
#' roman_digit_to_number('M')
roman_digit_to_number <- function(rd){
    r_indices <- which(ROMANS == toupper(substr(rd,1,1)), arr.ind = TRUE)
    if(length(r_indices)==0){
        stop('"', rd, '" is not a valid Roman numeral')
    }
    return(c(1,5)[r_indices[1]]*10^(r_indices[2]-1))
}

#' Convert Roman numeral to its decimal equivalent
#'
#' @param roman Roman numeral (character string)
#'
#' @return corresponding decmal number
#' @export
#'
#' @examples
#' roman_to_decimal('MMXX')
roman_to_decimal <- function(roman){
    n <- nchar(roman)
    digit_values <- numeric(n)
    digits <- strsplit(roman, '')[[1]]
    # convert all characters to their decimal value
    for (i in seq_along(digits)){
        digit_values[i] <- roman_digit_to_number(digits[i])
    }
    # find those that are not in descending sorting order ....
    neg_digits <- c(digit_values[-1],0) > digit_values
    # ... and negate them
    digit_values[neg_digits] <- -digit_values[neg_digits]
    # then simply sum
    return(sum(digit_values))
}
