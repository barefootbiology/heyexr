// Includes/namespaces
#include <Rcpp.h>
#include <cstdint>
#include <ctime>

using namespace Rcpp;

//' @title
//' datetime_to_raw
//' @description
//' Covert a datetime value, encoded as an integer, to the binary representation
//' of Windows filetime to POSIXct.
//'
//' @param x Integer representing the seconds from the epoch
//'
//' @return a raw vector of 8 bytes representing the number of 100 nanosecond
//' units since 1601-01-01.
//'
//' @details
//' \code{raw_to_datetime} takes an raw vector encoding a 64-bit unsigned integer
//' This value is the number of 100 nanoseconds since 1601-01-01.
//'
//' @export
// [[Rcpp::export]]
RawVector datetime_to_raw (int sec_epoch) {
    time_t sec_epoch_ = (time_t) sec_epoch;

    uint64_t nsec_epoch;
    nsec_epoch = sec_epoch_ * 1000000000 / 100;

    // Offset in 100 nanosecond intervals between 1970-01-01 and 1601-01-01
    uint64_t nsec_offset = ((uint64_t)134774) * 24 * 60 * 60 * 1000 * 1000 * 10;

    uint64_t nsec = nsec_epoch + nsec_offset;

    RawVector src( 8 );

    uint8_t tmp[src.size()];

    std::memcpy(&tmp, &nsec, sizeof tmp);

    // // Copy the contents of src to a vector of 8 bit integers
    for(int i = 0; i < src.size(); i++) {
        src[i] = (Rbyte) tmp[i];
    }

    return(src);
}
