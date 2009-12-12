require 'rake/clean'

CLOBBER.include('./**/*.o')
CLOBBER.include('./**/*.hi')
CLOBBER.include('tests/Main')

SRC = Dir["./src/**/*.hs"] + Dir["./tests/**/*.hs"]
FOLDERS = "-isrc -itests"

task :compile do
  sh "ghc --make #{FOLDERS} tests/Main.hs"
end

task :tests => :compile do
  sh "./tests/Main"
end
