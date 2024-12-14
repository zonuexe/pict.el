# PICT support for Emacs

**[PICT]** is free software released by Microsoft and stands for "[Pairwise][All-pairs testing] Independent Combinatorial Testing."

This package can be run standalone, or run via org-babel from the `.org` document.

> [!NOTE]
> Learn more about PICT and [Pairwise (all-pairs) testing][All-pairs testing] in [the official documentation].
>> PICT generates test cases and test configurations. With PICT, you can generate tests that are more effective than manually generated tests and in a fraction of the time required by hands-on test case design.

## Install

For Emacs 29 and later, you can easily install it with `package-vc-install`.

```el
(package-vc-install
 '(pict :url "git@github.com:zonuexe/pict.el.git"
        :main-file "pict.el"))
```

## `pict.el`

`pict.el` contains major modes for editing PICT models and functions for building `pict` commands.
You can also change the symbols for your model's syntax using custom variables.

> [!WARNING]
> There is no standard extension for PICT model files, so they are not added transparently to the `auto-mode-alist`.
>
> If you have determined the convention for naming PICT model files, you may add them at your own risk.

## `ob-pict.el`

`ob-pict.el` contains [Org Babel] support functions for evaluting PICT model.

### Generate tests from `.org` files

[Org Babel] is known as a useful tool for implementing [literate programming][Literate programming].

Create a new `example.org` and paste the following code into it.

```org
* Check web pages by operating system, browser and language

#+begin_src pict
  OS: Windows, macOS, Debian, Android, iOS
  Browser: Firefox, Chrome, Safari
  Lang: English, Traditioal Chinese, Simplified Chinese, Japanese, Korean

  if [OS] NOT IN {"macOS", "iOS"} then [Browser] <> "Safari";
  if [OS] = "iOS" then [Browser] = "Safari";
#+end_src
```

> [!WARNING]
> If you don't see PICT syntax highlighting in your Emacs buffer, it's likely that `pict.el` is not set up correctly.

Move your cursor into this area of ​​code and press `C-c C-c` or `M-x org-babel-execute-maybe`.

If PICT and `pict.el` are set up correctly, you should see the following output:

``` org
#+RESULTS:
| OS      | Browser | Lang               |
|---------+---------+--------------------|
| Windows | Firefox | Traditioal Chinese |
| iOS     | Safari  | Korean             |
| Windows | Chrome  | Japanese           |
| Debian  | Firefox | Simplified Chinese |
| iOS     | Safari  | English            |
| macOS   | Chrome  | Korean             |
| Debian  | Chrome  | Korean             |
| Windows | Chrome  | Simplified Chinese |
| Debian  | Firefox | Japanese           |
| iOS     | Safari  | Simplified Chinese |
| Windows | Firefox | Korean             |
| Debian  | Chrome  | Traditioal Chinese |
| Android | Chrome  | Simplified Chinese |
| iOS     | Safari  | Traditioal Chinese |
| Android | Firefox | English            |
| macOS   | Safari  | Traditioal Chinese |
| Android | Chrome  | Traditioal Chinese |
| macOS   | Firefox | English            |
| Debian  | Chrome  | English            |
| Android | Chrome  | Korean             |
| iOS     | Safari  | Japanese           |
| Windows | Chrome  | English            |
| macOS   | Chrome  | Japanese           |
| Android | Chrome  | Japanese           |
| macOS   | Chrome  | Simplified Chinese |
```

### Show statistics

Want to see the model's stats?  Try adding `:var stats=1` to the header line.

``` diff
-#+begin_src pict
+#+begin_src pict :var stats=1
```

If you specify `:var stats="only"`, only statistics will be output.`

### Results of Evaluation

> [!NOTE]
> Please see [Results of Evaluation (The Org Manual)][Results of Evaluation].

`ob-pict` supports many results, with the following exceptions:

 * **Collection**
   * `value`, `output`: It has no meaning to PICT so it will simply be ignored.
 * **Format**
   * None of them are implemented.

### Output styles

#### `style="transpose"`

The table is displayed in a transposed format. This style is particularly useful for creating decision tables, where readability and organization of rules are essential. However, note that transposed tables can become quite wide horizontally, which may affect layout and readability in some cases.

```org
#+begin_src pict :var style="transpose"
  Foo: 1, 2, 3
  Bar: A, B, C
  Buz: xxx, yyy, zzz
#+end_src

#+RESULTS:
| Rule |   1 |   2 |   3 |   4 |   5 |   6 |   7 |   8 |   9 |
|------+-----+-----+-----+-----+-----+-----+-----+-----+-----|
| Foo  |   3 |   2 |   3 |   2 |   2 |   1 |   3 |   1 |   1 |
| Bar  |   B |   B |   C |   A |   C |   B |   A |   A |   C |
| Buz  | yyy | xxx | zzz | zzz | yyy | zzz | xxx | yyy | xxx |
```

#### `style="matrix"`

The table is displayed in a matrix-style format, where conditions and values are represented as binary-like indicators (`T` for true). This layout is especially useful for visualizing combinations of conditions or rules in a compact, grid-like structure.

```org
#+begin_src pict :var style="matrix"
  Foo: 1, 2, 3
  Bar: A, B, C
  Buz: xxx, yyy, zzz
#+end_src

#+RESULTS:
| Rule      | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 |
|-----------+---+---+---+---+---+---+---+---+---|
| Foo = 1   |   |   |   |   |   | T |   | T | T |
| Foo = 2   |   | T |   | T | T |   |   |   |   |
| Foo = 3   | T |   | T |   |   |   | T |   |   |
| Bar = A   |   |   |   | T |   |   | T | T |   |
| Bar = B   | T | T |   |   |   | T |   |   |   |
| Bar = C   |   |   | T |   | T |   |   |   | T |
| Buz = xxx |   | T |   |   |   |   | T |   | T |
| Buz = yyy | T |   |   |   | T |   |   | T |   |
| Buz = zzz |   |   | T | T |   | T |   |   |   |
```

## Copyright

This package is licensed under [GPL Version 3][GPL-3.0] or later.

```
Copyright (C) 2024  USAMI Kenta

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
```

[All-pairs testing]: https://en.wikipedia.org/wiki/All-pairs_testing
[GPL-3.0]: https://www.gnu.org/licenses/gpl-3.0.html
[Literate programming]: https://en.wikipedia.org/wiki/Literate_programming
[Org Babel]: https://orgmode.org/worg/org-contrib/babel/intro.html
[PICT]: https://github.com/microsoft/pict
[Results of Evaluation]: https://www.gnu.org/software/emacs/manual/html_node/org/Results-of-Evaluation.html
[the official documentation]: https://github.com/Microsoft/pict/blob/main/doc/pict.md
