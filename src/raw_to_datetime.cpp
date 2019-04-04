// Includes/namespaces
#include <Rcpp.h>
#include <cstdint>
#include <ctime>

using namespace Rcpp;

//' @title
//' raw_to_datetime
//' @description
//' Covert the binary representation of Windows filetime to POSIXct
//'
//' @param x a raw vector of 8 bytes representing the number of 100 nanosecond
//' units since 1601-01-01.
//'
//'
//' @details
//' \code{raw_to_datetime} takes an raw vector encoding a 64-bit unsigned integer
//' This value is the number of 100 nanoseconds since 1601-01-01.
//'
//' @export
// [[Rcpp::export]]
Datetime raw_to_datetime (RawVector src) {
    // Temporary variable to hold the raw bytes from src
    uint8_t tmp[src.size()];

    // Variable to hold 100 nanosecond units.
    uint64_t nsec = 0;

    // Copy the contents of src to a vector of 8 bit integers
    for(int i = 0; i < src.size(); i++) {
        tmp[i] = (uint8_t) src[i];
    }

    std::memcpy(&nsec, &tmp, sizeof nsec);

    // Offset in 100 nanosecond intervals between 1970-01-01 and 1601-01-01.
    // (days * hours * minutes * seconds * milliseconds * microseconds *
    //  100-nanosecond intervals)
    uint64_t nsec_offset = ((uint64_t)134774) * 24 * 60 * 60 * 1000 * 1000 * 10;

    uint64_t nsec_epoch = nsec - nsec_offset;

    // Convert from 100-nanosecond intervals from epoch to seconds from epoch
    time_t sec_epoch;
    sec_epoch = (time_t) (nsec_epoch * 100 / 1000000000);

    return((int) sec_epoch);
}
