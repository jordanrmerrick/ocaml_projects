# 1 "book/duniverse/mdx.dev/lib/compat.ml"
(*
 * Copyright (c) 2018 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

module String = struct
  include String


# 36 "book/duniverse/mdx.dev/lib/compat.ml"
end

module Filename = struct
  include Filename

# 61 "book/duniverse/mdx.dev/lib/compat.ml"
end

module List = struct
  include List


# 88 "book/duniverse/mdx.dev/lib/compat.ml"
end

module Warnings = struct
  include Warnings

# 97 "book/duniverse/mdx.dev/lib/compat.ml"
end


# 109 "book/duniverse/mdx.dev/lib/compat.ml"
let init_path () = Compmisc.init_path ()
