import "jest";

import { parseSExp } from "../s_exp";

describe("s_exp", () => {
    test("S expressions", () => {
        expect(parseSExp("()")).toEqual(null);
        expect(parseSExp("(foo bar baz)")).toEqual({
            car: "foo",
            cdr: {
                car: "bar",
                cdr: {
                    car: "baz",
                    cdr: null,
                },
            },
        });
        expect(parseSExp("(foo (bar baz) . qux)")).toEqual({
            car: "foo",
            cdr: {
                car: {
                    car: "bar",
                    cdr: {
                        car: "baz",
                        cdr: null,
                    },
                },
                cdr: "qux",
            },
        });
    });
});
