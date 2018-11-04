import "jest";

import { parseJson } from "../json";

describe("json", () => {
    test("null", () => {
        expect(parseJson("null")).toEqual(null);
    });
    test("number", () => {
        expect(parseJson("42")).toEqual(42);
    });
    test("string", () => {
        expect(parseJson("\"foobar\"")).toEqual("foobar");
        expect(parseJson("\"foo\\nbar\"")).toEqual("foo\nbar");
    });
    test("boolean", () => {
        expect(parseJson("true")).toEqual(true);
        expect(parseJson("false")).toEqual(false);
    });
    test("array", () => {
        expect(parseJson("[42, \"foobar\", true]")).toEqual([42, "foobar", true]);
    });
    test("object", () => {
        expect(parseJson("{ \"a\": 42, \"b\": \"foobar\", \"c\": true }"))
            .toEqual({ a: 42, b: "foobar", c: true });
    });
    test("complex data", () => {
        const data = `
            {
                "success": true,
                "messages": [
                    {
                        "id": 0,
                        "text": "hello"
                    },
                    {
                        "id": 1,
                        "text": "what's up?"
                    },
                    {
                        "id": 2,
                        "text": "ok"
                    }
                ]
            }
        `;
        expect(parseJson(data)).toEqual({
            "success": true,
            "messages": [
                {
                    "id": 0,
                    "text": "hello",
                },
                {
                    "id": 1,
                    "text": "what's up?",
                },
                {
                    "id": 2,
                    "text": "ok",
                },
            ],
        });
    });
});
