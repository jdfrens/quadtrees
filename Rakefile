require 'rake/clean'

CLOBBER.include('./**/*.o')
CLOBBER.include('./**/*.hi')
CLOBBER.include('tests/Main')

SRC = Dir["./src/**/*.hs"] + Dir["./tests/**/*.hs"]

task :compile => SRC.map { |f| f.gsub(".hs", ".o") } do
  sh "ghc --make -isrc -itests tests/Main.hs"
end

task :tests => :compile do
  sh "./tests/Main"
end

rule '.o' => '.hs' do |t|
  sh "ghc -isrc -itests -c #{t.source}"
end
