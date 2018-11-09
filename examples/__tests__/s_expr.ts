import "jest";

import { parseSExpr } from "../s_expr";

describe("s_expr", () => {
    test("S-expressions", () => {
        expect(parseSExpr("()")).toEqual(null);
        expect(parseSExpr("(foo bar baz)")).toEqual({
            car: "foo",
            cdr: {
                car: "bar",
                cdr: {
                    car: "baz",
                    cdr: null,
                },
            },
        });
        expect(parseSExpr("(foo (bar baz) . qux)")).toEqual({
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
