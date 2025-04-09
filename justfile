update-tests:
  just update-test tests.json
  just update-test spec_tests.json

update-test f:
  curl -L https://raw.githubusercontent.com/json-patch/json-patch-tests/refs/heads/master/{{f}} | jq > {{f}}
