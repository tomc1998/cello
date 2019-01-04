#pragma once

#include <vector>
#include <nonstd/string_view.hpp>
#include <nonstd/optional.hpp>

namespace cello {
	struct prog_arg_parse_exception {
		std::string error_msg;
	};

	struct prog_arg {
		nonstd::string_view name;
		nonstd::optional<nonstd::string_view> val;
		bool is_flag;
    prog_arg(nonstd::string_view name,
        nonstd::optional<nonstd::string_view> val,
        bool is_flag)
      : name(name), val(val), is_flag(is_flag) {};
	};

	/** A wrapper around a vector of args, for querying the args */
	struct prog_args {
		std::vector<prog_arg> arg_list;
		/** Returns nullptr if the flag doesn't exist. */
		const prog_arg* find_flag(nonstd::string_view flag_name) const;
		int num_positional() const;
		const prog_arg& get_positional(int pos) const;
	};

	/** Throws an arg_parse_exception containing an error msg */
	prog_args parse_prog_arg_list(int argc, const char** argv);
}
