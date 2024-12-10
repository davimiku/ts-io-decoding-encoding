import { Result } from './result.js'

type ShapeTag =
  | 'unknown'
  | 'boolean'
  | 'number'
  | 'string'
  | 'bigint'
  | 'array'
  | 'record'
  | 'struct'
  | 'union'
  | 'optional'
  | 'custom'

type DecodeError = string | { [key: string]: string | DecodeError }
type DecodeResult<T> = Result<T, DecodeError>

interface Shape<T, Input = unknown, Output = T> {
  readonly __tag: ShapeTag
  readonly decode: (input: Input) => DecodeResult<T>
  readonly encode: (value: T) => Output
}

// `any` is used here for other shapes to extend this shape
// so that any value can be used for `T` since subtyping is checked
// both covariantly and contravariantly
// eslint-disable-next-line @typescript-eslint/no-explicit-any
type BaseShape = Shape<any, unknown, unknown>

interface ShapeUnknown extends Shape<unknown> {
  readonly __tag: 'unknown'
}

interface ShapeBoolean extends Shape<boolean> {
  readonly __tag: 'boolean'
}

interface ShapeNumber extends Shape<number> {
  readonly __tag: 'number'
}

interface ShapeString extends Shape<string> {
  readonly __tag: 'string'
}

interface ShapeBigInt extends Shape<bigint> {
  readonly __tag: 'bigint'
}

/**
 * Given a `Shape` type, extracts the `Output` type from it
 */
type OutputOf<S extends BaseShape> =
  S extends Shape<infer _T, infer _Input, infer Output> ? Output : never

interface ShapeArray<S extends BaseShape>
  extends Shape<Infer<S>[], unknown, OutputOf<S>[]> {
  readonly __tag: 'array'
}

interface ShapeRecord<S extends BaseShape>
  extends Shape<
    Record<string, Infer<S>>,
    unknown,
    Record<string, OutputOf<S>>
  > {
  readonly __tag: 'record'
}

type StructFields = Record<string, BaseShape>

interface ShapeStruct<Fields extends StructFields>
  extends Shape<InferStruct<Fields>, unknown, OutputOfStruct<Fields>> {
  readonly __tag: 'struct'
}

type UnionVariants = Record<string, BaseShape>

interface ShapeUnion<Variants extends UnionVariants>
  extends Shape<InferUnion<Variants>, unknown, OutputOfUnion<Variants>> {
  readonly __tag: 'union'
}

interface ShapeCustom<T, Input, Output> extends Shape<T, Input, Output> {
  readonly __tag: 'custom'
}

interface ShapeOptional<S extends BaseShape>
  extends Shape<
    Infer<S> | undefined,
    Infer<S> | undefined,
    OutputOf<S> | undefined
  > {
  readonly __tag: 'optional'
}

// The `T` constraint is `any` so that `T` can be narrower that `unknown`
// for custom decoders (checked contravariantly)
// eslint-disable-next-line @typescript-eslint/no-explicit-any
export type Infer<S extends Shape<any, any, unknown>> = S extends ShapeUnknown
  ? unknown
  : S extends ShapeBoolean
    ? boolean
    : S extends ShapeNumber
      ? number
      : S extends ShapeString
        ? string
        : S extends ShapeBigInt
          ? bigint
          : S extends ShapeOptional<infer InnerShape>
            ? Infer<InnerShape> | undefined
            : S extends ShapeArray<infer ElementShape>
              ? Infer<ElementShape>[]
              : S extends ShapeRecord<infer ValueShape>
                ? Record<string, Infer<ValueShape>>
                : S extends ShapeStruct<infer Fields>
                  ? InferStruct<Fields>
                  : S extends ShapeUnion<infer Variants>
                    ? InferUnion<Variants>
                    : S extends ShapeCustom<
                          infer T,
                          infer _Input,
                          infer _Output
                        >
                      ? T
                      : never

type InferStruct<Fields extends StructFields> = {
  [Key in keyof Fields]: Infer<Fields[Key]>
}

type OutputOfStruct<Fields extends StructFields> = {
  [Key in keyof Fields]: OutputOf<Fields[Key]>
}

type InferUnion<Variants extends UnionVariants> = {
  [Key in keyof Variants]: [Key, Infer<Variants[Key]>]
}[keyof Variants]

type OutputOfUnion<Variants extends UnionVariants> = {
  [Key in keyof Variants]: [Key, OutputOf<Variants[Key]>]
}[keyof Variants]

function typeOf(input: unknown): string {
  if (input === null) {
    return 'null'
  }
  return typeof input
}

const identity = <T>(t: T) => t

export const unknown: ShapeUnknown = {
  __tag: 'unknown',
  decode: (input: unknown) => Result.success(input),
  encode: identity,
} as const

export const boolean: ShapeBoolean = {
  __tag: 'boolean',
  decode: (input: unknown): DecodeResult<boolean> => {
    if (typeof input !== 'boolean') {
      return Result.error(`expected 'boolean', got '${typeOf(input)}'`)
    }
    return Result.success(input)
  },
  encode: identity,
} as const

export const number: ShapeNumber = {
  __tag: 'number',
  decode(input: unknown): DecodeResult<number> {
    if (typeof input !== 'number') {
      return Result.error(`expected 'number', got '${typeOf(input)}'`)
    }
    if (Number.isNaN(input)) {
      return Result.error(`expected 'number', got 'NaN'`)
    }
    if (!Number.isFinite(input)) {
      return Result.error(`expected finite 'number', got '${input}'`)
    }

    return Result.success(input)
  },
  encode: identity,
} as const

export const string: ShapeString = {
  __tag: 'string',
  decode(input: unknown): DecodeResult<string> {
    if (typeof input !== 'string') {
      return Result.error(`expected 'string', got '${typeOf(input)}'`)
    }
    return Result.success(input)
  },
  encode: identity,
} as const

export const bigint: ShapeBigInt = {
  __tag: 'bigint',
  decode(input: unknown): DecodeResult<bigint> {
    if (typeof input === 'bigint') {
      return Result.success(input)
    } else if (typeof input === 'number') {
      if (Number.isNaN(input)) {
        return Result.error(`expected bigint or number, got 'NaN'`)
      }
      if (!Number.isFinite(input)) {
        return Result.error(`expected finite number, got ${input}`)
      }
      return Result.success(BigInt(input))
    }
    return Result.error(`expected 'bigint' or 'number', got '${typeOf(input)}'`)
  },
  encode: identity,
} as const

export function array<S extends BaseShape>(elementShape: S): ShapeArray<S> {
  return {
    __tag: 'array',
    decode(input: unknown): DecodeResult<Infer<S>[]> {
      if (!Array.isArray(input)) {
        return Result.error(`expected 'Array', got '${typeOf(input)}'`)
      }

      const errors: DecodeError = {}
      const output = Array<Infer<S>>(input.length)

      for (let i = 0; i < input.length; i++) {
        const result = elementShape.decode(input[i])
        if (Result.isSuccess(result)) {
          output[i] = result.value as Infer<S>
        } else {
          errors[i] = result.error
        }
      }

      if (Object.keys(errors).length > 0) {
        return Result.error(errors)
      }

      return Result.success(output)
    },
    encode(array: Infer<S>[]) {
      return array.map((element) => elementShape.encode(element) as OutputOf<S>)
    },
  } as const
}

export function record<S extends BaseShape>(valueShape: S): ShapeRecord<S> {
  return {
    __tag: 'record',
    decode(input: unknown): DecodeResult<Record<string, Infer<S>>> {
      if (!input || typeof input !== 'object' || Array.isArray(input)) {
        return Result.error(`expected 'Object', got '${typeOf(input)}'`)
      }

      const errors: DecodeError = {}
      const output: Record<string, Infer<S>> = {}

      for (const [key, value] of Object.entries(input)) {
        const result = valueShape.decode(value)
        if (Result.isSuccess(result)) {
          output[key] = result.value as Infer<S>
        } else {
          errors[key] = result.error
        }
      }

      if (Object.keys(errors).length > 0) {
        return Result.error(errors)
      }

      return Result.success(output)
    },
    encode(record: Record<string, Infer<S>>): Record<string, OutputOf<S>> {
      return Object.fromEntries(
        Object.entries(record).map(([key, value]) => [
          key,
          valueShape.encode(value) as OutputOf<S>,
        ]),
      )
    },
  } as const
}

const UnknownRecord = record(unknown)

export function struct<Fields extends StructFields>(
  fieldShapes: Fields,
): ShapeStruct<Fields> {
  return {
    __tag: 'struct',
    decode(input: unknown): DecodeResult<InferStruct<Fields>> {
      return Result.flatMap(UnknownRecord.decode(input), (record) =>
        decodeRecordToStruct(record, fieldShapes),
      )
    },
    encode(struct: InferStruct<Fields>) {
      return Object.fromEntries(
        Object.entries(struct).map(([key, value]) => {
          return [key, fieldShapes[key].encode(value)]
        }),
      ) as OutputOfStruct<Fields>
    },
  } as const
}

function decodeRecordToStruct<Fields extends StructFields>(
  data: Record<string, unknown>,
  fieldShapes: Fields,
): DecodeResult<InferStruct<Fields>> {
  const output = {} as InferStruct<Fields>
  const errors: DecodeError = {}

  for (const [key, shape] of Object.entries(fieldShapes)) {
    const result = shape.decode(data[key])
    if (Result.isSuccess(result)) {
      output[key as keyof Fields] = result.value as Infer<Fields[keyof Fields]>
    } else {
      errors[key] = result.error
    }
  }

  if (Object.keys(errors).length > 0) {
    return Result.error(errors)
  }

  return Result.success(output)
}

export function union<Variants extends UnionVariants>(
  variants: Variants,
): ShapeUnion<Variants> {
  return {
    __tag: 'union',
    decode(input: unknown): DecodeResult<InferUnion<Variants>> {
      if (!Array.isArray(input) || input.length !== 2) {
        return Result.error(
          'expected a 2-tuple with the tag as the first element',
        )
      }

      const [tag, value] = input as [unknown, unknown]

      if (typeof tag !== 'string') {
        return Result.error(
          `expected the tag to be a 'string', got ${typeOf(tag)}`,
        )
      }

      if (!Object.keys(variants).includes(tag)) {
        return Result.error(
          'expected the tag to match one of the provided variants',
        )
      }

      const result = variants[tag].decode(value)

      return Result.mapError(
        Result.map(result, (value) => [
          tag,
          value as Infer<Variants[typeof tag]>,
        ]),
        (error) => ({ [tag]: error }),
      )
    },
    encode(union: InferUnion<Variants>) {
      const [tag, value] = union

      return [tag, variants[tag].encode(value)] as OutputOfUnion<Variants>
    },
  }
}

export function custom<T, Input, Output>(
  decode: (input: Input) => DecodeResult<T>,
  encode: (value: T) => Output,
): ShapeCustom<T, Input, Output> {
  return {
    __tag: 'custom',
    decode,
    encode,
  }
}

// `any` needs to be used for `T` to work in both directions
// eslint-disable-next-line @typescript-eslint/no-explicit-any
export function optional<S extends Shape<any, any, unknown>>(
  shape: S,
): ShapeOptional<S> {
  return {
    __tag: 'optional',
    decode(input: Infer<S> | undefined) {
      if (typeof input === 'undefined') {
        return Result.success(input)
      }
      return shape.decode(input)
    },
    encode(value: Infer<S> | undefined): OutputOf<S> | undefined {
      if (typeof value === 'undefined') {
        return undefined
      }
      return shape.encode(value) as OutputOf<S>
    },
  }
}

if (import.meta.vitest) {
  const { test, expect, describe } = import.meta.vitest

  // https://www.totaltypescript.com/how-to-test-your-types
  type Expect<T extends true> = T
  // prettier-ignore
  // eslint-disable-next-line @typescript-eslint/no-unnecessary-type-parameters
  type Equal<X, Y> = (<T>() => T extends X ? 1 : 2) extends (<T>() => T extends Y ? 1 : 2)
      ? true
      : false

  /**
   * Vitest's assertion functions don't make use of the TypeScript
   * [asserts keyword](https://devblogs.microsoft.com/typescript/announcing-typescript-3-7/#assertion-functions)
   * so this helper is useful to unwrap after doing our own assertion.
   */
  function unsafeUnwrap<T, Err>(result: Result<T, Err>): T {
    if (Result.isSuccess(result)) {
      return result.value
    }
    throw new Error('Unwrapped an Error result')
  }

  /**
   * Vitest's assertion functions don't make use of the TypeScript
   * [asserts keyword](https://devblogs.microsoft.com/typescript/announcing-typescript-3-7/#assertion-functions)
   * so this helper is useful to unwrap after doing our own assertion.
   */
  function unsafeUnwrapError<T, Err>(result: Result<T, Err>): Err {
    if (Result.isError(result)) {
      return result.error
    }
    throw new Error('Unwrapped a Success result')
  }

  /**
   * Custom shape with operations for testing:
   * - decode: unknown => boolean
   * - encode: boolean => string
   */
  const BoolToString = custom(boolean.decode, (val: boolean) => String(val))

  /**
   * Custom shape with operations for testing:
   * - decode: unknown => Map<string, string>
   * - encode: Map<string, string> => Record<string, string>
   */
  const StringMap = custom(
    (input) =>
      Result.map(
        record(string).decode(input),
        (rec) => new Map(Object.entries(rec)),
      ),
    (map: Map<string, string>) => Object.fromEntries(map),
  )

  describe('Unknown', () => {
    test('Unknown decoding', () => {
      const result = unknown.decode('no idea')
      expect(Result.isSuccess(result))

      const actual = unsafeUnwrap(result)
      expect(actual).toStrictEqual('no idea')

      type Expected = unknown
      type Actual = Infer<typeof unknown>

      type _Test = Expect<Equal<Expected, Actual>>
    })
  })

  describe('Boolean', () => {
    test.each([
      [true, true],
      [false, false],
    ])('Boolean decoding sucess: %s -> %s', (input, expected) => {
      const result = boolean.decode(input)
      expect(Result.isSuccess(result))

      const actual = unsafeUnwrap(result)
      expect(actual).toStrictEqual(expected)

      type Expected = boolean
      type Actual = Infer<typeof boolean>

      type _Test = Expect<Equal<Expected, Actual>>
    })
  })

  describe('Number', () => {
    test('Number type inference', () => {
      expect(true).toBe(true)

      type Expected = number
      type Actual = Infer<typeof number>

      type _Test = Expect<Equal<Expected, Actual>>
    })

    test.each([
      [16, 16],
      [0, 0],
      [-0, -0],
      [-16, -16],
    ])('Number decoding success: %f -> %f', (input, expected) => {
      const result = number.decode(input)
      expect(Result.isSuccess(result))

      const actual = unsafeUnwrap(result)
      expect(actual).toStrictEqual(expected)
    })

    test.each([
      ['yolo', "expected 'number', got 'string'"],
      [NaN, "expected 'number', got 'NaN'"],
      [+Infinity, "expected finite 'number', got 'Infinity'"],
      [-Infinity, "expected finite 'number', got '-Infinity'"],
      [null, "expected 'number', got 'null'"],
    ])('Number decoding failure: %s', (input, expected) => {
      const result = number.decode(input)
      expect(Result.isError(result))

      const actual = unsafeUnwrapError(result)
      expect(actual).toStrictEqual(expected)
    })
  })

  describe('String', () => {
    test('String type inference', () => {
      expect(true).toBe(true)

      type Expected = string
      type Actual = Infer<typeof string>

      type _Test = Expect<Equal<Expected, Actual>>
    })

    test.each([
      ['', ''],
      ['hello', 'hello'],
    ])('String decoding: %s -> %s', (input, expected) => {
      const result = string.decode(input)
      expect(Result.isSuccess(result))

      const actual = unsafeUnwrap(result)
      expect(actual).toStrictEqual(expected)
    })
  })

  describe('Array', () => {
    const StringArray = array(string)
    test('Array inference', () => {
      expect(true).toBe(true)

      type Expected = string[]
      type Actual = Infer<typeof StringArray>

      type _Test = Expect<Equal<Expected, Actual>>
    })

    test.each([
      [[], []],
      [
        ['a', 'b', 'c'],
        ['a', 'b', 'c'],
      ],
    ])('Array decoding success', (input, expected) => {
      const result = StringArray.decode(input)
      expect(Result.isSuccess(result))

      const actual = unsafeUnwrap(result)
      expect(actual).toStrictEqual(expected)
    })

    test('nested Array decoding', () => {
      const StringArrayArray = array(array(string))

      const result = StringArrayArray.decode([
        ['a', 'b'],
        ['c', 'd'],
        ['e', 'f'],
      ])
      expect(Result.isSuccess(result))

      const actual = unsafeUnwrap(result)
      expect(actual).toStrictEqual([
        ['a', 'b'],
        ['c', 'd'],
        ['e', 'f'],
      ])

      type Expected = string[][]
      type Actual = Infer<typeof StringArrayArray>

      type _Test = Expect<Equal<Expected, Actual>>
    })

    test.each([
      [[0], { '0': "expected 'string', got 'number'" }],
      [
        ['0', 1, '2', 3],
        {
          '1': "expected 'string', got 'number'",
          '3': "expected 'string', got 'number'",
        },
      ],
    ])('Array decoding failure', (input, expected) => {
      const result = StringArray.decode(input)
      expect(Result.isError(result))

      const actual = unsafeUnwrapError(result)
      expect(actual).toStrictEqual(expected)
    })

    test('Custom Array encoding', () => {
      const BoolStringArray = array(BoolToString)

      const input = [true, true, false, true]

      const decodeResult = BoolStringArray.decode(input)
      expect(Result.isSuccess(decodeResult))

      const decoded: boolean[] = unsafeUnwrap(decodeResult)
      expect(decoded).toStrictEqual([true, true, false, true])

      const encoded: string[] = BoolStringArray.encode(decoded)
      expect(encoded).toStrictEqual(['true', 'true', 'false', 'true'])
    })
  })

  describe('Record', () => {
    test('Record decoding', () => {
      const StringRecord = record(string)

      const result = StringRecord.decode({ a: 'aa', b: 'bb' })
      expect(Result.isSuccess(result))

      const actual = unsafeUnwrap(result)

      expect(actual).toStrictEqual({
        a: 'aa',
        b: 'bb',
      })

      type Expected = Record<string, string>
      type Actual = Infer<typeof StringRecord>

      type _Test = Expect<Equal<Expected, Actual>>
    })

    test('nested Record decoding', () => {
      const StringRecordRecord = record(record(string))

      const result = StringRecordRecord.decode({
        a: { aa: 'aaa' },
        b: { bb: 'bbb' },
      })
      expect(Result.isSuccess(result))

      const actual = unsafeUnwrap(result)
      expect(actual).toStrictEqual({ a: { aa: 'aaa' }, b: { bb: 'bbb' } })

      type Expected = Record<string, Record<string, string>>
      type Actual = Infer<typeof StringRecordRecord>

      type _Test = Expect<Equal<Expected, Actual>>
    })
  })

  describe('Struct', () => {
    const Point = struct({
      x: number,
      y: number,
    })

    test('Struct decoding success', () => {
      const result = Point.decode({ x: 1, y: 2 })
      expect(Result.isSuccess(result))

      const actual = unsafeUnwrap(result)
      expect(actual).toStrictEqual({ x: 1, y: 2 })

      type Expected = {
        x: number
        y: number
      }
      type Actual = Infer<typeof Point>

      type _Test = Expect<Equal<Expected, Actual>>
    })

    test('nested Struct decoding success', () => {
      const Point = struct({
        x: number,
        y: number,
      })
      const NestedPoint = struct({
        start: Point,
        end: Point,
      })

      const result = NestedPoint.decode({
        start: { x: 1, y: 2 },
        end: { x: 10, y: 20 },
      })
      expect(Result.isSuccess(result))

      const actual = unsafeUnwrap(result)
      expect(actual).toStrictEqual({
        start: { x: 1, y: 2 },
        end: { x: 10, y: 20 },
      })

      type Expected = {
        start: {
          x: number
          y: number
        }
        end: {
          x: number
          y: number
        }
      }
      type Actual = Infer<typeof NestedPoint>

      type _Test = Expect<Equal<Expected, Actual>>
    })

    test.each([
      [{ x: 'hello world', y: 2 }, { x: "expected 'number', got 'string'" }],
      [{ y: 2 }, { x: "expected 'number', got 'undefined'" }],
      [
        { y: 'hello world' },
        {
          x: "expected 'number', got 'undefined'",
          y: "expected 'number', got 'string'",
        },
      ],
    ])('Struct decoding failure', (input, expected) => {
      const result = Point.decode(input)
      expect(Result.isError(result))

      const actual = unsafeUnwrapError(result)
      expect(actual).toStrictEqual(expected)
    })

    test('Struct type inference', () => {
      expect(true).toBe(true)

      type Expected = {
        n: number
        s: string
        a: boolean[]
        r: Record<string, number>
      }
      type Actual = InferStruct<{
        n: ShapeNumber
        s: ShapeString
        a: ShapeArray<ShapeBoolean>
        r: ShapeRecord<ShapeNumber>
      }>

      type _Test = Expect<Equal<Expected, Actual>>
    })
  })

  describe('Union', () => {
    const NumOrStr = union({
      num: number,
      str: string,
    })

    test.each([
      [
        ['num', 16],
        ['num', 16],
      ],
      [
        ['str', 'hello'],
        ['str', 'hello'],
      ],
    ])('Union decoding success: %s', (input, expected) => {
      const result = NumOrStr.decode(input)
      expect(Result.isSuccess(result))

      const actual = unsafeUnwrap(result)
      expect(actual).toStrictEqual(expected)

      type Expected = ['num', number] | ['str', string]
      type Actual = Infer<typeof NumOrStr>

      type _Test = Expect<Equal<Expected, Actual>>
    })

    test.each([
      [
        ['click', { location: { x: 1, y: 2 }, isDoubleClick: false }],
        ['click', { location: { x: 1, y: 2 }, isDoubleClick: false }],
      ],
      [
        [
          'drag',
          { start: { x: 0, y: 100 }, end: { x: 100, y: 100 }, duration: 300 },
        ],
        [
          'drag',
          { start: { x: 0, y: 100 }, end: { x: 100, y: 100 }, duration: 300 },
        ],
      ],
    ])('nested Union decoding', (input, expected) => {
      const Point = struct({
        x: number,
        y: number,
      })
      const ClickEvent = struct({
        location: Point,
        isDoubleClick: boolean,
      })
      const DragEvent = struct({
        start: Point,
        end: Point,
        duration: number,
      })
      const MouseEvent = union({
        click: ClickEvent,
        drag: DragEvent,
      })

      const result = MouseEvent.decode(input)
      expect(Result.isSuccess(result))

      const actual = unsafeUnwrap(result)
      expect(actual).toStrictEqual(expected)

      type Expected =
        | ['click', Infer<typeof ClickEvent>]
        | ['drag', Infer<typeof DragEvent>]
      type Actual = Infer<typeof MouseEvent>

      type _Test = Expect<Equal<Expected, Actual>>
    })

    test.each([
      [['num', '5'], { num: "expected 'number', got 'string'" }],
      [['str', null], { str: "expected 'string', got 'null'" }],
    ])('Union decoding failure', (input, expected) => {
      const result = NumOrStr.decode(input)
      expect(Result.isError(result))

      const actual = unsafeUnwrapError(result)
      expect(actual).toStrictEqual(expected)
    })

    test('Union type inference', () => {
      expect(true).toBe(true)

      type Expected =
        | ['x', number]
        | ['s', string]
        | ['a', boolean[]]
        | ['r', Record<string, number>]

      type Actual = InferUnion<{
        x: ShapeNumber
        s: ShapeString
        a: ShapeArray<ShapeBoolean>
        r: ShapeRecord<ShapeNumber>
      }>

      type _Test = Expect<Equal<Expected, Actual>>
    })
  })

  describe('Optional', () => {
    test.each([
      [3, 3],
      [undefined, undefined],
      // eslint-disable-next-line no-sparse-arrays
      [, ,],
    ])('Optional number success: %s', (input, expected) => {
      const OptionalNumber = optional(number)

      const result = OptionalNumber.decode(input)
      expect(Result.isSuccess(result))

      const actual = unsafeUnwrap(result)
      expect(actual).toStrictEqual(expected)
    })

    test('Optional type inference', () => {
      expect(true).toBe(true)

      const _OptionalNumber = optional(number)

      type Expected = number | undefined
      type Actual = Infer<typeof _OptionalNumber>

      type _Test = Expect<Equal<Expected, Actual>>
    })
  })

  describe('Custom', () => {
    const regexes = [
      /^(?<year>\d{4})-(?<month>\d{2})-(?<day>\d{2})$/, // YYYY-MM-DD
      /^(?<month>\d{2})\/(?<day>\d{2})\/(?<year>\d{4})$/, // MM/DD/YYYY
      /^(?<month>\d{2})_(?<day>\d{2})_(?<year>\d{4})$/, // MM_DD_YYYY
    ]
    function customDateDecoder(input: string): DecodeResult<Date> {
      for (const regex of regexes) {
        const match = input.match(regex)
        if (match) {
          // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
          const year = Number.parseInt(match.groups!['year'])
          // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
          const month = Number.parseInt(match.groups!['month'])
          // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
          const day = Number.parseInt(match.groups!['day'])
          return Result.success(new Date(year, month - 1, day)) // month starts at zero...
        }
      }
      return Result.error(`unable to parse ${input} into a Date`)
    }
    const CustomDate = custom(customDateDecoder, (date) => date.toISOString())

    test('custom decoder inference', () => {
      type Expected = Date
      type Actual = Infer<typeof CustomDate>

      type _Test = Expect<Equal<Expected, Actual>>
    })

    test.each([
      ['2024-11-18', new Date(2024, 10, 18)],
      ['11/18/2024', new Date(2024, 10, 18)],
      ['11_18_2024', new Date(2024, 10, 18)],
    ])('custom decoder functionality', (input, expected) => {
      const result = CustomDate.decode(input)
      expect(Result.isSuccess(result))

      const actual = unsafeUnwrap(result)
      expect(actual).toStrictEqual(expected)
    })

    test('Custom StringMap', () => {
      type Actual = Infer<typeof StringMap>
      type _Test = Expect<Equal<Map<string, string>, Actual>>
      const result: DecodeResult<Map<string, string>> = StringMap.decode({
        a: 'a',
        b: 'b',
        c: 'c',
      })
      expect(Result.isSuccess(result))

      const decoded: Map<string, string> = unsafeUnwrap(result)
      expect(decoded).toStrictEqual(
        new Map([
          ['a', 'a'],
          ['b', 'b'],
          ['c', 'c'],
        ]),
      )

      const encoded: Record<string, string> = StringMap.encode(decoded)
      expect(encoded).toStrictEqual({ a: 'a', b: 'b', c: 'c' })
    })
  })
}
