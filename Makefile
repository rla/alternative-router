version:=$(shell swipl -q -s pack -g 'version(V),writeln(V)' -t halt)
packfile=arouter-$(version).tgz
remote=www-data@packs.rlaanemets.com:/sites/packs.rlaanemets.com/public/alternative-router

package: test
	tar cvzf $(packfile) prolog tests examples pack.pl README.md LICENSE

test:
	swipl -s tests/tests.pl -g run_tests,halt -t 'halt(1)'

doc:
	swipl -q -t 'doc_save(prolog, [doc_root(doc),format(html),title(arouter),if(true),recursive(true)])'

upload: doc package
	scp $(packfile) $(remote)/$(packfile)
	rsync -avz -e ssh doc $(remote)

.PHONY: test package doc upload
