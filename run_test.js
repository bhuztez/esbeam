class AssertionError extends Error {
  constructor() {
    super();
    this.name = 'AssertionError';
  }
}


let TestUtil = {
    assert(expr) {
        if (expr !== true)
            throw new AssertionError();
    }
};


function run_test(test) {
    if (typeof(test) === 'object') {
        return [for (key of Object.keys(test))
                {name: key, result: run_test(test[key])}];
    } else if (typeof(test) === 'function') {
        try {
            test(TestUtil);
            return {result: "ok"};
        } catch (e if e instanceof AssertionError) {
            return {result: "assertion failed",
                    stack: e.stack};
        } catch(e) {
            return {result: "error",
                    name: e.name, message: e.message, stack: e.stack};
        }
    } else {
        throw new Error("invalid type");
    }
}


import * as tests from "tests/main.js";
print(JSON.stringify(run_test(tests)));
