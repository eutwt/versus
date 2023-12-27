#include <cpp11.hpp>
using namespace cpp11;

[[cpp11::register]]
data_frame get_diff_rows_int(integers vec_a, integers vec_b, integers idx_a, integers idx_b) {
    int n = idx_a.size();
    writable::integers row_a, row_b;

    for (int i = 0; i < n; ++i) {
        if (vec_a[idx_a[i] - 1] != vec_b[idx_b[i] - 1]) {
            row_a.push_back(idx_a[i]);
            row_b.push_back(idx_b[i]);
        }
    }
    writable::data_frame out({"row_a"_nm = row_a, "row_b"_nm = row_b});
    out.attr("class") = {"tbl_df", "tbl", "data.frame"};
    return std::move(out);
}


[[cpp11::register]]
data_frame get_diff_rows_dbl(doubles vec_a, doubles vec_b, integers idx_a, integers idx_b) {
    int n = idx_a.size();
    writable::integers row_a, row_b;

    for (int i = 0; i < n; ++i) {
        double a = vec_a[idx_a[i] - 1];
        double b = vec_b[idx_b[i] - 1];
        if (a != b) {
            if (R_IsNA(a) && R_IsNA(b)) {
                continue;
            }
            row_a.push_back(idx_a[i]);
            row_b.push_back(idx_b[i]);
        }
    }
    writable::data_frame out({"row_a"_nm = row_a, "row_b"_nm = row_b});
    out.attr("class") = {"tbl_df", "tbl", "data.frame"};
    return std::move(out);
}

[[cpp11::register]]
list split_matches(integers needles, integers haystack) {
    // split `matches` into 
    //   a = vector of rows only in `table_a` 
    //   b = vector of rows only in `table_b`
    //   common = tibble of indices from both tables
    int n = needles.size();
    writable::integers a, b, common_a, common_b;

    for (int i = 0; i < n; ++i) {
        if (needles[i] == NA_INTEGER) {
            b.push_back(haystack[i]);
        } else if (haystack[i] == NA_INTEGER) {
            a.push_back(needles[i]);
        } else {
            common_a.push_back(needles[i]);
            common_b.push_back(haystack[i]);
        }
    }
    writable::data_frame common({"a"_nm = common_a, "b"_nm = common_b});
    common.attr("class") = {"tbl_df", "tbl", "data.frame"};
    writable::list out({"common"_nm = common, "a"_nm = a, "b"_nm = b});
    return std::move(out);
}

