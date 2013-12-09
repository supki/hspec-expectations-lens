# A sample Guardfile
# More info at https://github.com/guard/guard#readme

guard :haskell, ghci_options: ["-ignore-dot-ghci"] do
  watch(%r{test/.+Spec\.l?hs$})
  watch(%r{src/.+\.l?hs$})
end
