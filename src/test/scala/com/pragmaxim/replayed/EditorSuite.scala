package com.pragmaxim.replayed

import utest._

object EditorSuite extends TestSuite with EditorHelper {

  val tests = TestSuite {
    "Navigate" - {

      "down" - {
        "should pass edge cases" - {
          "at MOF" - {
            "from longer to shorter" - prepare("|1|2|3|4|5|6|nl|a|b|c|^|>|>|v|", Seq("123456", "abc"), (1, "abc"), None)
            "from shorter to longer" - prepare("|a|b|c|nl|1|2|3|4|5|6|<|<|<|^|v|", Seq("abc", "123456"), (1, "123"), None)
          }
          "at EOF" - {
            "at EOL" - prepare("|a|v|", Seq("a"), (0, "a"), None)
            "at BOL" - prepare("|a|<|v|", Seq("a"), (0, "a"), None)
          }
        }

        "should support selection" - {

          "at MOF" - {
            "at BOL" - {
              "single" - prepare("|1|2|nl|3|4|<|<|^|_v|", Seq("12", "34"), (1, ""), Some("12"))
              "multiple" - prepare("|1|2|nl|3|4|nl|5|6|<|<|^|^|_v|_v|", Seq("12", "34", "56"), (2, ""), Some("1234"))
            }

            "at MOL" - {
              "single" - prepare("|1|2|nl|3|4|<|^|_v|", Seq("12", "34"), (1, "3"), Some("23"))
              "multiple" - prepare("|1|2|nl|3|4|nl|5|6|<|^|^|_v|_v|", Seq("12", "34", "56"), (2, "5"), Some("2345"))
            }
          }

          "at EOF" - {
            "at BOL" - prepare("|1|2|<|<|_v|", Seq("12"), (0, "12"), Some("12"))
            "at MOL" - prepare("|1|2|<|_v|", Seq("12"), (0, "12"), Some("2"))
          }

        }
      }

      "up" - {

        "should pass edge cases" - {
          "at MOF" - {
            "from shorter to longer" - prepare("|1|2|3|4|5|6|nl|a|b|c|^|", Seq("123456", "abc"), (0, "123"), None)
            "from longer to shorter" - prepare("|a|b|c|nl|1|2|3|4|5|6|^|", Seq("abc", "123456"), (0, "abc"), None)
          }
          "at BOF" - {
            "at BOL" - prepare("|a|<|^|", Seq("a"), (0, ""), None)
            "at MOL" - prepare("|a|^|", Seq("a"), (0, ""), None)
          }
        }

        "should support selection" - {
          "at MOF" - {
            "at BOL" - {
              "single" - prepare("|1|2|nl|3|4|<|<|_^|", Seq("12", "34"), (0, ""), Some("12"))
              "multiple" - prepare("|1|2|nl|3|4|nl|5|6|<|<|_^|_^|", Seq("12", "34", "56"), (0, ""), Some("1234"))
            }
            "at MOL" - {
              "single" - prepare("|1|2|nl|3|4|<|_^|", Seq("12", "34"), (0, "1"), Some("23"))
              "multiple" - prepare("|1|2|nl|3|4|nl|5|6|<|_^|_^|", Seq("12", "34", "56"), (0, "1"), Some("2345"))
            }
          }
          "at BOF" - {
            "at BOL" - prepare("|1|2|<|<|_^|", Seq("12"), (0, ""), None)
            "at MOL" - prepare("|1|2|<|_^|", Seq("12"), (0, ""), Some("1"))
          }
        }
      }


      "right" - {
        "should pass edge cases" - {
          "normally" - {
            "at MOL" - prepare("|a|b|c|<|<|>|", Seq("abc"), (0, "ab"), None)
            "at EOF" - prepare("|a|>|>|", Seq("a"), (0, "a"), None)
            "at MOF" - prepare("|a|nl|b|^|>|", Seq("a", "b"), (1, ""), None)
            "at empty MOF" - prepare("|nl|b|^|>|", Seq("", "b"), (1, ""), None)
          }

          "quickly" - {
            "MOL" - prepare("|1|2|3| |4|<<|>|~>|", Seq("123 4"), (0, "123"), None)
            "BOL" - prepare("|1|2|3| |4|<<|~>|", Seq("123 4"), (0, "123"), None)
            "skip single space" - prepare("|1| |2|<<|>|~>|", Seq("1 2"), (0, "1 "), None)
            "skip multi space" - prepare("|1| | | |2|<<|~>|~>|", Seq("1   2"), (0, "1   "), None)
            "at EOF" - prepare("|a|~>|~>|", Seq("a"), (0, "a"), None)
            "at MOF" - prepare("|a|nl|b|^|~>|", Seq("a", "b"), (1, ""), None)
            "at empty MOF" - prepare("|a|nl|^|~>|~>|", Seq("a", ""), (1, ""), None)
          }
        }

        "should support selection" - {

          "normally" - {
            "at EOF" - {
              "at MOL" - prepare("|1|2|3|4|5|<<|>|_>|_>|_>|", Seq("12345"), (0, "1234"), Some("234"))
              "at EOL" - prepare("|1|2|_>|_>|", Seq("12"), (0, "12"), None)
              //TODO phantom #12721 "after ending space" - prepare("|1|2| |<|_>|", Seq("12 "), (0, "12 "), Some(" "))
            }

            "at MOF" - {
              "at MOL" - prepare("|1|2|3|nl|4|5|^|_>|_>|_>|", Seq("123", "45"), (1, "4"), Some("34"))
              "at EOL" - {
                "line not empty" - prepare("|1|2|nl|3|4|^|_>|_>|_>|", Seq("12", "34"), (1, "34"), Some("34"))
                "line empty" - prepare("|nl|3|4|^|_>||", Seq("", "34"), (1, ""), Some(""))
              }
              "over multiple lines" - prepare("|1|2|nl|3|4|nl|5|6|^|^|_>|_>|_>|_>|_>|", Seq("12", "34", "56"), (2, "5"), Some("345"))
            }
          }

          "quickly" - {
            "to EOL" - prepare("|1|2|3|4|5|<<|>|~_>|", Seq("12345"), (0, "12345"), Some("2345"))
            "before space" - prepare("|1|2|3| |5|<<|>|~_>|", Seq("123 5"), (0, "123"), Some("23"))
            //TODO phantom #12721 "after spaces" - prepare("|1|2|3| | |6|<<|>|~_>|~_>|", Seq("123  6"), (0, "123  "), Some("23  "))
            "at EOF" - prepare("|1|2|~_>|", Seq("12"), (0, "12"), None)
            "before ending space" - prepare("|1|2| |<<|~_>|", Seq("12 "), (0, "12"), Some("12"))
            //TODO phantom #12721 "after ending space" - prepare("|1|2| |<<|~_>|~_>|", Seq("12 "), (0, "12 "), Some("12 "))
            "to next line" - prepare("|1|2|nl|3|4|^|~_>|~_>|", Seq("12", "34"), (1, "34"), Some("34"))
            //TODO phantom #12721 "general case" - prepare("|1|2| | |5|6|7|<<|~_>|~_>|~_>|", Seq("12  567"), (0, "12  567"), Some("12  567"))
          }
        }

      }

      "left" - {

        "should pass edge cases" - {
          "normally" - {
            "at MOL" - prepare("|a|b|c|<|<|", Seq("abc"), (0, "a"), None)
            "at BOF" - prepare("|a|<|<|", Seq("a"), (0, ""), None)
            "at MOF" - prepare("|a|nl|b|<|<|", Seq("a", "b"), (0, "a"), None)
            "at empty MOF" - prepare("|a|nl|<|", Seq("a", ""), (0, "a"), None)
          }

          "quickly" - {
            "MOL" - prepare("|1| |2|3|4|<|<~|", Seq("1 234"), (0, "1 "), None)
            "EOL" - prepare("|1| |2|3|4|<~|", Seq("1 234"), (0, "1 "), None)
            "skip single space" - prepare("|1| |2|<|<~|", Seq("1 2"), (0, "1"), None)
            "skip multi space" - prepare("|1| | | |2|<~|<~|", Seq("1   2"), (0, "1"), None)
            "at BOF" - prepare("|a|<~|<~|", Seq("a"), (0, ""), None)
            "at MOF" - prepare("|a|nl|b|<|<~|", Seq("a", "b"), (0, "a"), None)
            "at empty MOF" - prepare("|a|nl|<~|", Seq("a", ""), (0, "a"), None)
          }
        }

        "should support selection" - {
          "normally" - {
            "at BOF" - {
              "at MOL" - prepare("|1|2|3|4|5|<|_<|_<|_<|", Seq("12345"), (0, "1"), Some("234"))
              "at BOL" - prepare("|1|2|<|<|_<|_<|", Seq("12"), (0, ""), None)
            }

            "at MOF" - {
              "at MOL" - prepare("|1|2|3|nl|4|5|_<|_<|_<|_<|", Seq("123", "45"), (0, "12"), Some("345"))
              "at BOL" - {
                "line not empty" - prepare("|1|2|nl|3|4|<|<|_<|_<|", Seq("12", "34"), (0, "1"), Some("2"))
                "line empty" - prepare("|1|2|nl|_<|_<|", Seq("12", ""), (0, "1"), Some("2"))
              }
              "over multiple lines" - prepare("|1|2|nl|3|4|nl|5|6|<|_<|_<|_<|_<|_<|_<|", Seq("12", "34", "56"), (0, "1"), Some("2345"))
            }
          }
          "quickly" - {
            "to BOL" - prepare("|1|2|3|4|5|<|<_~|", Seq("12345"), (0, ""), Some("1234"))
            "before space" - prepare("|1| |3|4|<_~|", Seq("1 34"), (0, "1 "), Some("34"))
            //TODO phantom #12721 "after spaces" - prepare("|1| | |4|5|6|<_~|<_~|", Seq("1  456"), (0, "1"), Some("  456"))
            "at BOF" - prepare("|1|2|<<|<_~|", Seq("12"), (0, ""), None)
            "before starting space" - prepare("| |3|4|<_~|", Seq(" 34"), (0, " "), Some("34"))
            //TODO phantom #12721 "after starting space" - prepare("| | |3|4|<_~|<_~|", Seq("  34"), (0, ""), Some("  34"))
            "to prev line" - prepare("|1|2|nl|3|4|<_~|<_~|", Seq("12", "34"), (0, "12"), Some("34"))
            //TODO phantom #12721 "general case" - prepare("|1|2| | |5|6|7|<_~|<_~|<_~|", Seq("12  567"), (0, ""), Some("12  567"))
          }
        }
      }

      "end" - {
        "should pass edge cases" - {
          "at EOL" - prepare("|a|b|>>|>>|", Seq("ab"), (0, "ab"), None)
          "at BOL" - prepare("|a|b|<<|>>|", Seq("ab"), (0, "ab"), None)
          "at MOL" - prepare("|a|b|c|<|<|>>|", Seq("abc"), (0, "abc"), None)
        }

        "should support selection" - {
          "at EOL" - prepare("|a|b|_>>|_>>|", Seq("ab"), (0, "ab"), None)
          "at BOL" - prepare("|1|2|3|4|5|<<|_>>|", Seq("12345"), (0, "12345"), Some("12345"))
          "at MOL" - prepare("|1|2|3|4|5|<<|>|_>>|", Seq("12345"), (0, "12345"), Some("2345"))
        }

      }

      "home" - {
        "should pass edge cases" - {
          "at EOL" - prepare("|a|b|<<|", Seq("ab"), (0, ""), None)
          "at BOL" - prepare("|a|b|<|<|<<|", Seq("ab"), (0, ""), None)
          "at MOL" - prepare("|a|b|c|<|<<|", Seq("abc"), (0, ""), None)
        }

        "should support selection" - {
          "at EOL" - prepare("|a|b|_<<|", Seq("ab"), (0, ""), Some("ab"))
          "at BOL" - prepare("|1|2|3|4|5|<<|_<<|", Seq("12345"), (0, ""), None)
          "at MOL" - prepare("|1|2|3|4|5|<|_<<|", Seq("12345"), (0, ""), Some("1234"))
        }

      }

      "pageUp" - {
        "should pass edge cases" - {
          "BOF" - {
            "BOL" - prepare("|a|b|<<|^^|", Seq("ab"), (0, ""), None)
            "MOL" - prepare("|a|b|^^|", Seq("ab"), (0, ""), None)
          }

          "MOF" - {
            "BOL" - prepare("|1|2|nl|3|4|nl|5|6|<<|^^|", Seq("12", "34", "56"), (0, ""), None)
            "MOL" - prepare("|1|2|nl|3|4|nl|5|6|<|^^|", Seq("12", "34", "56"), (0, ""), None)
          }
        }

        "should support selection" - {
          "BOF" - {
            "BOL" - prepare("|a|b|<<|_^^|", Seq("ab"), (0, ""), None)
            "MOL" - prepare("|a|b|_^^|", Seq("ab"), (0, ""), Some("ab"))
          }

          "MOF" - {
            "BOL" - prepare("|1|2|nl|3|4|nl|5|6|<<|_^^|", Seq("12", "34", "56"), (0, ""), Some("1234"))
            "MOL" - prepare("|1|2|nl|3|4|nl|5|6|<|_^^|", Seq("12", "34", "56"), (0, ""), Some("12345"))
          }
        }
      }

      "pageDown" - {
        "should pass edge cases" - {
          "EOF" - {
            "EOL" - prepare("|a|b|vv|", Seq("ab"), (0, "ab"), None)
            "MOL" - prepare("|a|b|<|vv|", Seq("ab"), (0, "ab"), None)
          }

          "MOF" - {
            "BOL" - prepare("|1|2|nl|3|4|nl|5|6|<<|^|^|vv|", Seq("12", "34", "56"), (2, "56"), None)
            "MOL" - prepare("|1|2|nl|3|4|nl|5|6|<|^|vv|", Seq("12", "34", "56"), (2, "56"), None)
          }
        }

        "should support selection" - {
          "EOF" - {
            "EOL" - prepare("|a|b|_vv|", Seq("ab"), (0, "ab"), None)
            "MOL" - prepare("|a|b|<|_vv|", Seq("ab"), (0, "ab"), Some("b"))
          }

          "MOF" - {
            "BOL" - prepare("|1|2|nl|3|4|nl|5|6|<<|^|^|_vv|", Seq("12", "34", "56"), (2, "56"), Some("123456"))
            "MOL" - prepare("|1|2|nl|3|4|nl|5|6|<|^|_vv|", Seq("12", "34", "56"), (2, "56"), Some("456"))
          }
        }
      }

      "selection combinations" - {
        "at BOL" - {
          "up and down" - prepare("|1|2|nl|3|4|nl|5|6|<|<|_^|_^|_v|_v|", Seq("12", "34", "56"), (2, ""), None)
          "down and up" - prepare("|1|2|nl|3|4|nl|5|6|<|<|^|^|_v|_v|_^|_^|", Seq("12", "34", "56"), (0, ""), None)
        }
        "at MOL" - {
          "up and down" - prepare("|1|2|nl|3|4|nl|5|6|<|_^|_^|_v|_v|", Seq("12", "34", "56"), (2, "5"), None)
          "down and up" - prepare("|1|2|nl|3|4|nl|5|6|<|^|^|_v|_v|_^|_^|", Seq("12", "34", "56"), (0, "1"), None)
        }
        "at EOL" - {
          "up and down" - prepare("|1|2|nl|3|4|nl|5|6|_^|_^|_v|_v|", Seq("12", "34", "56"), (2, "56"), None)
          "down and up" - prepare("|1|2|nl|3|4|nl|5|6|^|^|_v|_v|_^|_^|", Seq("12", "34", "56"), (0, "12"), None)
        }

        "expand shrink exit" - {
          "left and right" - prepare("|1|2|3|4|5|<|_<|_<|_>|_>|_>|", Seq("12345"), (0, "12345"), Some("5"))
          "right and left" - prepare("|1|2|3|4|5|<<|>|_>|_>|_<|_<|_<|", Seq("12345"), (0, ""), Some("1"))
          "up and down" - prepare("|1|2|nl|3|4|nl|5|6|nl|7|8|<|^|_^|_^|_v|_v|_v|", Seq("12", "34", "56", "78"), (3, "7"), Some("67"))
          "down and up" - prepare("|1|2|nl|3|4|nl|5|6|nl|7|8|<|^|^|_v|_v|_^|_^|_^|", Seq("12", "34", "56", "78"), (0, "1"), Some("23"))
        }

        "switch" - {
          "left and end" - prepare("|1|2|3|4|5|<|_<|_<|_>>|", Seq("12345"), (0, "12345"), Some("5"))
          "right and home" - prepare("|1|2|3|4|5|<<|>|_>|_>|_<<|", Seq("12345"), (0, ""), Some("1"))
          "up and down" - prepare("|1|2|3|nl|4|5|6|nl|7|8|9|<|<|_^|_>|_v|", Seq("123", "456", "789"), (2, "78"), Some("8"))
          "down and up" - prepare("|1|2|3|nl|4|5|6|nl|7|8|9|<|^|^|_v|_<|_^|", Seq("123", "456", "789"), (0, "1"), Some("2"))
        }
      }

      "Mouse click should" - {

        "point before character" - {
          val leftBHalfHit = pl(Array(Meter.charWidths('a'), Meter.charWidths('b') / 2 - 1D)) // one pixel before 'b' half
          prepare(s"|a|b|c|[0,$leftBHalfHit]|", Seq("abc"), (0, "a"), None)
        }

        "point after character" - {
          val rightBHalfHit = pl(Array(Meter.charWidths('a'), Meter.charWidths('b') / 2 + 1D)) // one pixel after 'b' half
          prepare(s"|a|b|c|[0,$rightBHalfHit]|", Seq("abc"), (0, "ab"), None)
        }

        "point at EOL if it occurred beyond" - prepare(s"|a|b|c|<|<|[0,200.0]|", Seq("abc"), (0, "abc"), None)

        "point at BOL if it occurred anywhere at empty line" - prepare(s"|a|b|nl|^|[1,100.0]|", Seq("ab", ""), (1, ""), None)

        "point at EOF if it occurred out of line scope" - prepare(s"|a|b|nl|1|2|<|^|[,100.0]|", Seq("ab", "12"), (1, "12"), None)
      }

      "Undo should survive pointer change" - {
        prepare("|1|2|3|4|5|rm|rm|<|<|z|z|", Seq("12345"), (0, "12345"), None)
      }

      "Redo should survive pointer change" - {
        prepare("|1|2|3|4|5|rm|rm|<|<|z|z|y|y|", Seq("123"), (0, "123"), None)
      }

    }


    "Input" - {

      "should apply" - prepare("|a|b|c|", Seq("abc"), (0, "abc"), None)

      "should undo" - {
        "single" - prepare("|a|b|c|z|", Seq("ab"), (0, "ab"), None)
        "multiple" - prepare("|a|b|c|z|z|", Seq("a"), (0, "a"), None)
      }

      "should redo" - {
        "single" - prepare("|a|b|c|z|y|", Seq("abc"), (0, "abc"), None)
        "multiple" - prepare("|a|b|c|z|z|y|y|", Seq("abc"), (0, "abc"), None)
      }
    }

    "Remove" - {

      "should apply to" - {
        "BOF" - prepare("|a|<|rm|rm|rm|rm|", Seq("a"), (0, ""), None)
        "BOL" - prepare("|a|nl|b|<|rm|", Seq("ab"), (0, "a"), None)
        "BOL with empty line" - prepare("|a|nl|rm|", Seq("a"), (0, "a"), None)
        "MOL" - prepare("|a|b|c|rm|rm|", Seq("a"), (0, "a"), None)
      }

      "should undo at" - {

        "single" - {
          "BOL" - prepare("|a|nl|b|<|rm|z|", Seq("a", "b"), (1, ""), None)
          "BOL with empty line" - prepare("|a|nl|rm|z|", Seq("a", ""), (1, ""), None)
          "MOL" - prepare("|a|b|c|<|rm|z|", Seq("abc"), (0, "ab"), None)
        }

        "multiple" - {
          "combined" - prepare("|a|b|nl|c|<|rm|rm|z|z|", Seq("ab", "c"), (1, ""), None)
        }

      }

      "should redo at" - {

        "single" - {
          "BOL" - prepare("|a|nl|b|<|rm|z|y|", Seq("ab"), (0, "a"), None)
          "BOL with empty line" - prepare("|a|nl|rm|z|y|", Seq("a"), (0, "a"), None)
          "MOL" - prepare("|a|b|c|<|rm|z|y|", Seq("ac"), (0, "a"), None)
        }

        "multiple" - {
          "combined" - prepare("|a|b|nl|c|<|rm|rm|z|z|y|y|", Seq("ac"), (0, "a"), None)
        }
      }
    }

    "Delete" - {

      "should apply to" - {

        "EOF" - prepare("|a|del|del|del|del|del|", Seq("a"), (0, "a"), None)

        "EOL" - prepare("|a|nl|b|^|del|", Seq("ab"), (0, "a"), None)

        "EOL with empty line" - prepare("|a|nl|^|>|del|", Seq("a"), (0, "a"), None)

        "MOL" - prepare("|a|b|c|<|<|del|", Seq("ac"), (0, "a"), None)
      }

      "should undo at" - {

        "single" - {
          "EOL" - prepare("|a|nl|b|^|del|z|", Seq("a", "b"), (0, "a"), None)
          "MOL" - prepare("|a|b|c|<|<|del|z|", Seq("abc"), (0, "a"), None)
        }

        "multiple" - {
          "EOL" - prepare("|a|nl|b|^|del|del|z|z|", Seq("a", "b"), (0, "a"), None)
          "MOL" - prepare("|a|b|c|<|<|del|del|z|z|", Seq("abc"), (0, "a"), None)
        }

      }

      "should redo at" - {

        "single" - {
          "EOL" - prepare("|a|nl|b|^|del|z|y|", Seq("ab"), (0, "a"), None)
          "MOL" - prepare("|a|b|c|<|<|del|z|y|", Seq("ac"), (0, "a"), None)
        }

        "multiple" - {
          "MOL" - prepare("|a|b|c|<|<|del|del|z|z|y|y|", Seq("a"), (0, "a"), None)
          "combined" - prepare("|a|b|nl|c|^|del|del|z|z|", Seq("ab", "c"), (0, "a"), None)
        }
      }

    }

    "Enter" - {

      "should apply to" - {
        "EOL" - prepare("|a|nl|", Seq("a", ""), (1, ""), None)
        "MOL" - prepare("|a|b|<|nl|", Seq("a", "b"), (1, ""), None)
      }

      "should undo at" - {

        "single" - {
          "EOL" - prepare("|a|nl|z|", Seq("a"), (0, "a"), None)
          "MOL" - prepare("|a|b|<|nl|z|", Seq("ab"), (0, "a"), None)
        }

        "multiple" - {
          "EOL" - prepare("|a|nl|nl|z|z|", Seq("a"), (0, "a"), None)
          "MOL" - prepare("|a|b|<|nl|nl|z|z|", Seq("ab"), (0, "a"), None)
        }
      }

      "should redo at" - {

        "single" - {
          "EOL" - prepare("|a|nl|z|y|", Seq("a", ""), (1, ""), None)
          "MOL" - prepare("|a|b|<|nl|z|y|", Seq("a", "b"), (1, ""), None)
        }

        "multiple" - {
          "EOL" - prepare("|a|nl|nl|z|z|y|y|", Seq("a", "", ""), (2, ""), None)

          "MOL" - prepare("|a|b|<|nl|nl|z|z|y|y|", Seq("a", "", "b"), (2, ""), None)
        }
      }
    }

    "Replace" - {

      "should apply to" - {
        "single line from" - {
          "start" - {
            "BOL" - prepare("|1|2|3|4|5|<|<|<|_<|_<|a|", Seq("a345"), (0, "a"), None)
            "BOL revert" - prepare("|1|2|3|4|5|<|<|<|_<|_<|a|z|z|", Seq("12345"), (0, ""), None)
            "MOL" - prepare("|1|2|3|4|5|<|_<|_<|b|", Seq("12b5"), (0, "12b"), None)
            "MOL revert" - prepare("|1|2|3|4|5|<|_<|_<|b|z|z|", Seq("12345"), (0, "12"), None)
            "EOL" - prepare("|1|2|3|4|5|_<|_<|_<|c|", Seq("12c"), (0, "12c"), None)
            "EOL revert" - prepare("|1|2|3|4|5|_<|_<|_<|c|z|z|", Seq("12345"), (0, "12"), None)
          }
          "end" - {
            "BOL" - prepare("|1|2|3|4|5|<<|_>|_>|a|", Seq("a345"), (0, "a"), None)
            "BOL revert" - prepare("|1|2|3|4|5|<<|_>|_>|a|z|z|", Seq("12345"), (0, "12"), None)
            "MOL" - prepare("|1|2|3|4|5|<<|>|>|_>|_>|b|", Seq("12b5"), (0, "12b"), None)
            "MOL revert" - prepare("|1|2|3|4|5|<<|>|>|_>|_>|b|z|z|", Seq("12345"), (0, "1234"), None)
            "EOL" - prepare("|1|2|3|4|5|<<|>|>|_>|_>|_>|c|", Seq("12c"), (0, "12c"), None)
            "EOL revert" - prepare("|1|2|3|4|5|<<|>|>|_>|_>|_>|c|z|z|", Seq("12345"), (0, "12345"), None)
          }
        }

        "multiple lines from" - {
          "start" - {
            "BOL" - prepare("|1|2|3|nl|4|5|6|nl|7|8|9|<|_<|_<|_^|_^|a|", Seq("a9"), (0, "a"), None)
            "BOL revert" - prepare("|1|2|3|nl|4|5|6|nl|7|8|9|<|_<|_<|_^|_^|a|z|z|", Seq("123", "456", "789"), (0, ""), None)
            "MOL" - prepare("|1|2|3|nl|4|5|6|<|_<|_^|b|", Seq("1b6"), (0, "1b"), None)
            "MOL revert" - prepare("|1|2|3|nl|4|5|6|<|_<|_^|b|z|z|", Seq("123", "456"), (0, "1"), None)
            "EOL" - prepare("|1|2|3|nl|4|5|6|nl|7|8|9|_<|_<|_<|_^|c|", Seq("123", "c"), (1, "c"), None)
            "EOL revert" - prepare("|1|2|3|nl|4|5|6|nl|7|8|9|_<|_<|_<|_^|c|z|z|", Seq("123", "456", "789"), (1, ""), None)
          }

          "end" - {
            "BOL" - prepare("|1|2|3|nl|4|5|6|<<|^|_>|_v|a|", Seq("a56"), (0, "a"), None)
            "BOL revert" - prepare("|1|2|3|nl|4|5|6|<<|^|_>|_v|a|z|z|", Seq("123", "456"), (1, "4"), None)
            "MOL" - prepare("|1|2|3|nl|4|5|6|<|<|^|_v|_>|b|", Seq("1b6"), (0, "1b"), None)
            "MOL revert" - prepare("|1|2|3|nl|4|5|6|<|<|^|_v|_>|b|z|z|", Seq("123", "456"), (1, "45"), None)
            "EOL" - prepare("|1|2|3|nl|4|5|6|nl|7|8|9|<<|^|^|_v|_v|_v|c|", Seq("c"), (0, "c"), None)
            "EOL revert" - prepare("|1|2|3|nl|4|5|6|nl|7|8|9|<<|^|^|_v|_v|_v|c|z|z|", Seq("123", "456", "789"), (2, "789"), None)
          }
        }
      }

    }

    "Paste" - {
      "should apply to" - {
        "single line at" - {
          "BOL" - prepare("|1|2|<<|[abc]|", Seq("abc12"), (0, "abc"), None)
          "BOL revert" - prepare("|1|2|<<|[abc]|z|", Seq("12"), (0, ""), None)
          "MOL" - prepare("|1|2|<|[abc]|", Seq("1abc2"), (0, "1abc"), None)
          "MOL revert" - prepare("|1|2|<|[abc]|z|", Seq("12"), (0, "1"), None)
          "EOL" - prepare("|1|2|[abc]|", Seq("12abc"), (0, "12abc"), None)
          "EOL revert" - prepare("|1|2|[abc]|z|", Seq("12"), (0, "12"), None)
        }

        "multiple lines at" - {
          "BOL" - prepare("|1|2|<<|[ab\ncd]|", Seq("ab", "cd12"), (1, "cd"), None)
          "BOL revert" - prepare("|1|2|<<|[ab\ncd]|z|", Seq("12"), (0, ""), None)
          "MOL" - prepare("|1|2|<|[ab\ncd]|", Seq("1ab", "cd2"), (1, "cd"), None)
          "MOL revert" - prepare("|1|2|<|[ab\ncd]|z|", Seq("12"), (0, "1"), None)
          "EOL" - prepare("|1|2|[ab\ncd]|", Seq("12ab", "cd"), (1, "cd"), None)
          "EOL revert" - prepare("|1|2|[ab\ncd]|z|", Seq("12"), (0, "12"), None)
        }
      }
    }
  }
}
