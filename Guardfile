# A sample Guardfile
# More info at https://github.com/guard/guard#readme

guard :haskell, cmd: "cabal repl spec --ghc-options=-ignore-dot-ghci", all_on_start: true do
  watch(%r{test/.+Spec\.l?hs$})
  watch(%r{src/.+\.l?hs$})
  watch(%r{\.cabal$})
end
