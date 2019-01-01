#pragma once

/**
   Looks at a string literal, replaces all the escape characters with their
   actual bytes (for example, \n with the newline) then returns a string view for
   that memory. This function allocates, and will just leak the memory (since
   the string views don't own it).

   Returns nullopt if the string ends with a \.

   @params s - This shouldn't be surrounded with "".

   TODO Handle UTF-8 (oops)
*/
nonstd::optional<nonstd::string_view> escape_all(nonstd::string_view s) {
  unsigned computed_final_length = 0;
  for (unsigned ii = 0; ii < s.size(); ++ii) {
    if (s[ii] == '\\') {
      if (ii == s.size() - 1) { return nonstd::nullopt; }
      switch (s[ii+1]) {
      case 'n': // newline
        ii += 1; // skip 1
        break;
      }
    }
    computed_final_length += 1;
  }

  char* final_string = new char[computed_final_length];
  unsigned curr_char = 0;
  for (unsigned ii = 0; ii < s.size(); ++ii) {
    if (s[ii] == '\\') {
      switch (s[ii+1]) {
      case 'n': // newline
        final_string[curr_char] = 10;
        ii += 1; // skip 1
        break;
      }
    } else {
      final_string[curr_char] = s[ii];
    }
    curr_char += 1;
  }

  return nonstd::string_view(final_string, computed_final_length);
}
