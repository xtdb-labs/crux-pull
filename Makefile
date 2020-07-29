.PHONY: test

test:
	echo "crux-pull:<span foreground='#ff3'>TEST</span>" > /tmp/crux-pull-test-status
	clojure -Atest && echo "crux-pull:PASS" > /tmp/crux-pull-test-status || echo "<span foreground='red'>crux-pull:FAIL</span>" > /tmp/crux-pull-test-status

watch:
	find . -name "*.clj" | entr make test
