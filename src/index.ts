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

type Shape<Input, T1, T2, Output> = Decoder<Input, T1> & Encoder<T2, Output>
type BaseShape = Shape<unknown, unknown, never, unknown>

type Decoder<Input, T1> = {
  readonly __tag: ShapeTag
  readonly decode: (input: Input) => DecodeResult<T1>
}

type Encoder<T2, Output> = {
  readonly __tag: ShapeTag
  readonly encode: (value: T2) => Output
}

interface ShapeUnknown extends BaseShape {
  readonly __tag: 'unknown'
}

interface ShapeBoolean extends Shape<unknown, boolean, never, boolean> {
  readonly __tag: 'boolean'
}

interface ShapeNumber extends Shape<unknown, number, never, number> {
  readonly __tag: 'number'
}

interface ShapeString extends Shape<unknown, string, never, string> {
  readonly __tag: 'string'
}

interface ShapeBigInt extends Shape<unknown, bigint, never, bigint> {
  readonly __tag: 'bigint'
}

interface ShapeArray<S extends BaseShape>
  extends Shape<unknown, InferDecoded<S>[], InferDecoded<S>[], InferEncoded<S>[]> {
  readonly __tag: 'array'
}

interface ShapeRecord<S extends BaseShape>
  extends Shape<
    unknown,
    Record<string, InferDecoded<S>>,
    Record<string, InferDecoded<S>>,
    Record<string, InferEncoded<S>>
  > {
  readonly __tag: 'record'
}

type StructFields = Record<string, BaseShape>

interface ShapeStruct<Fields extends StructFields>
  extends Shape<
    unknown,
    InferDecodedStruct<Fields>,
    InferDecodedStruct<Fields>,
    InferEncodedStruct<Fields>
  > {
  readonly __tag: 'struct'
}

type UnionVariants = Record<string, BaseShape>

interface ShapeUnion<Variants extends UnionVariants>
  extends Shape<
    unknown,
    InferDecodedUnion<Variants>,
    InferDecodedUnion<Variants>,
    InferEncodedUnion<Variants>
  > {
  readonly __tag: 'union'
}

interface ShapeCustom<Input, T1, T2, Output> extends Shape<Input, T1, T2, Output> {
  readonly __tag: 'custom'
}

interface ShapeOptional<Input, T1, T2, Output>
  extends Shape<Input, T1 | undefined, T2 | undefined, Output | undefined> {
  readonly __tag: 'optional'
}

export type InferDecoded<S extends Decoder<never, unknown>> = S extends ShapeUnknown
  ? unknown
  : S extends ShapeBoolean
    ? boolean
    : S extends ShapeNumber
      ? number
      : S extends ShapeString
        ? string
        : S extends ShapeBigInt
          ? bigint
          : S extends ShapeOptional<infer _Input, infer T1, infer _T2, infer _Output>
            ? T1 | undefined
            : S extends ShapeArray<infer ElementShape>
              ? InferDecoded<ElementShape>[]
              : S extends ShapeRecord<infer ValueShape>
                ? Record<string, InferDecoded<ValueShape>>
                : S extends ShapeStruct<infer Fields>
                  ? InferDecodedStruct<Fields>
                  : S extends ShapeUnion<infer Variants>
                    ? InferDecodedUnion<Variants>
                    : S extends ShapeCustom<infer _Input, infer T1, infer _T2, infer _Output>
                      ? T1
                      : never

export type InferEncoded<S extends Encoder<never, unknown>> = S extends ShapeUnknown
  ? unknown
  : S extends ShapeBoolean
    ? boolean
    : S extends ShapeNumber
      ? number
      : S extends ShapeString
        ? string
        : S extends ShapeBigInt
          ? bigint
          : S extends ShapeOptional<infer _Input, infer _T1, infer _T2, infer Output>
            ? Output | undefined
            : S extends ShapeArray<infer ElementShape>
              ? InferEncoded<ElementShape>[]
              : S extends ShapeRecord<infer ValueShape>
                ? Record<string, InferEncoded<ValueShape>>
                : S extends ShapeStruct<infer Fields>
                  ? InferEncodedStruct<Fields>
                  : S extends ShapeUnion<infer Variants>
                    ? InferEncodedUnion<Variants>
                    : S extends ShapeCustom<infer _Input, infer _T1, infer _T2, infer Output>
                      ? Output
                      : never

type StructFieldsDecode = Record<string, Decoder<unknown, unknown>>
/**
 * Helper (type) function to recursively decode fields of a struct.
 *
 * Needs to be separate from the (type) functions for encoding so that "T" is
 * treated covariantly.
 */
type InferDecodedStruct<Fields extends StructFieldsDecode> = {
  [Key in keyof Fields]: InferDecoded<Fields[Key]>
}

type StructFieldsEncode = Record<string, Encoder<never, unknown>>
/**
 * Helper (type) function to recursively decode fields of a struct.
 *
 * Needs to be separate from the (type) functions for encoding so that "T" is
 * treated contravariantly.
 */
type InferEncodedStruct<Fields extends StructFieldsEncode> = {
  [Key in keyof Fields]: InferEncoded<Fields[Key]>
}

type UnionVariantsDecode = Record<string, Decoder<unknown, unknown>>
/**
 * Helper (type) function to recursively decode variants of a tagged union.
 *
 * Needs to be separate from the (type) functions for encoding so that "T" is
 * treated covariantly.
 */
type InferDecodedUnion<Variants extends UnionVariantsDecode> = {
  [Key in keyof Variants]: [Key, InferDecoded<Variants[Key]>]
}[keyof Variants]

type UnionVariantsEncode = Record<string, Encoder<never, unknown>>
/**
 * Helper (type) function to recursively decode variants of a tagged union.
 *
 * Needs to be separate from the (type) functions for encoding so that "T" is
 * treated contravariantly.
 */
type InferEncodedUnion<Variants extends UnionVariantsEncode> = {
  [Key in keyof Variants]: [Key, InferEncoded<Variants[Key]>]
}[keyof Variants]

function typeOf(input: unknown): string {
  if (input === null) {
    return 'null'
  }
  return typeof input
}

function identity<T>(t: T): T {
  return t
}

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
    decode(input: unknown): DecodeResult<InferDecoded<S>[]> {
      if (!Array.isArray(input)) {
        return Result.error(`expected 'Array', got '${typeOf(input)}'`)
      }

      const errors: DecodeError = {}
      const output = Array<InferDecoded<S>>(input.length)

      for (let i = 0; i < input.length; i++) {
        const result = elementShape.decode(input[i])
        if (Result.isSuccess(result)) {
          output[i] = result.value as InferDecoded<S>
        } else {
          errors[i] = result.error
        }
      }

      if (Object.keys(errors).length > 0) {
        return Result.error(errors)
      }

      return Result.success(output)
    },
    encode(value: InferDecoded<S>[]): InferEncoded<S>[] {
      const encode = elementShape.encode as (value: unknown) => InferEncoded<S>
      return value.map((element) => encode(element))
    },
  } as const
}

export function record<S extends BaseShape>(valueShape: S): ShapeRecord<S> {
  return {
    __tag: 'record',
    decode(input: unknown): DecodeResult<Record<string, InferDecoded<S>>> {
      if (!input || typeof input !== 'object' || Array.isArray(input)) {
        return Result.error(`expected 'Object', got '${typeOf(input)}'`)
      }

      const errors: DecodeError = {}
      const output: Record<string, InferDecoded<S>> = {}

      for (const [key, value] of Object.entries(input)) {
        const result = valueShape.decode(value)
        if (Result.isSuccess(result)) {
          output[key] = result.value as InferDecoded<S>
        } else {
          errors[key] = result.error
        }
      }

      if (Object.keys(errors).length > 0) {
        return Result.error(errors)
      }

      return Result.success(output)
    },
    encode(val: Record<string, InferDecoded<S>>): Record<string, InferEncoded<S>> {
      const output: Record<string, InferEncoded<S>> = {}
      const encode = valueShape.encode as (value: unknown) => InferEncoded<S>

      for (const [key, value] of Object.entries(val)) {
        output[key] = encode(value)
      }
      return output
    },
  } as const
}

export function struct<Fields extends StructFields>(fieldShapes: Fields): ShapeStruct<Fields> {
  return {
    __tag: 'struct',
    decode(input: unknown): DecodeResult<InferDecodedStruct<Fields>> {
      return Result.flatMap(record(unknown).decode(input), (record) =>
        decodeRecordToStruct(record, fieldShapes),
      )
    },
    encode(struc: InferDecodedStruct<Fields>): InferEncodedStruct<Fields> {
      const output = {} as unknown as InferEncodedStruct<Fields>
      for (const [key, val] of Object.entries(struc)) {
        const k = key as keyof typeof fieldShapes
        const encode = fieldShapes[k].encode as (v: unknown) => InferEncoded<Fields[keyof Fields]>
        output[k] = encode(val)
      }
      return output
    },
  } as const
}

function decodeRecordToStruct<Fields extends StructFieldsDecode>(
  data: Record<string, unknown>,
  fieldShapes: Fields,
): DecodeResult<InferDecodedStruct<Fields>> {
  const output = {} as InferDecodedStruct<Fields>
  const errors: DecodeError = {}

  for (const [key, shape] of Object.entries(fieldShapes)) {
    const result = shape.decode(data[key])
    if (Result.isSuccess(result)) {
      output[key as keyof Fields] = result.value as InferDecoded<Fields[keyof Fields]>
    } else {
      errors[key] = result.error
    }
  }

  if (Object.keys(errors).length > 0) {
    return Result.error(errors)
  }

  return Result.success(output)
}

export function union<Variants extends UnionVariants>(variants: Variants): ShapeUnion<Variants> {
  return {
    __tag: 'union',
    decode(input: unknown): DecodeResult<InferDecodedUnion<Variants>> {
      if (!Array.isArray(input) || input.length !== 2) {
        return Result.error('expected a 2-tuple with the tag as the first element')
      }

      const [tag, value] = input as [unknown, unknown]

      if (typeof tag !== 'string') {
        return Result.error(`expected the tag to be a 'string', got ${typeOf(tag)}`)
      }

      if (!Object.keys(variants).includes(tag)) {
        return Result.error('expected the tag to match one of the provided variants')
      }

      const result = variants[tag].decode(value)

      return Result.mapError(
        Result.map(result, (value) => [tag, value as InferDecoded<Variants[typeof tag]>]),
        (error) => ({ [tag]: error }),
      )
    },
    encode(union: InferDecodedUnion<Variants>): InferEncodedUnion<Variants> {
      const [tag, value] = union

      const encode = variants[tag].encode as (v: unknown) => InferEncoded<Variants[keyof Variants]>
      const encodedValue = encode(value)

      return [tag, encodedValue]
    },
  }
}

export function custom<Input, T1, Output, T2 = T1>(
  decode: (input: Input) => DecodeResult<T1>,
  encode: (value: T2) => Output,
): ShapeCustom<Input, T1, T2, Output> {
  return {
    __tag: 'custom',
    decode,
    encode,
  }
}

export function optional<Input, T1, T2, Output>(
  shape: Shape<Input, T1, T2, Output>,
): ShapeOptional<Input, T1, T2, Output> {
  return {
    __tag: 'optional',
    decode: (input: Input) => {
      if (typeof input === 'undefined') {
        return Result.success(input)
      }
      return shape.decode(input)
    },
    encode(value: T2 | undefined) {
      if (typeof value === 'undefined') {
        return undefined
      }
      return shape.encode(value)
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
    (input) => Result.map(record(string).decode(input), (rec) => new Map(Object.entries(rec))),
    (map: Map<string, string>) => Object.fromEntries(map),
  )

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
        return Result.success(new Date(Date.UTC(year, month - 1, day))) // month starts at zero...
      }
    }
    return Result.error(`unable to parse ${input} into a Date`)
  }

  /**
   * Custom shape with operations for testing:
   * - decode: string => Date
   * - encode: Date => string
   */
  const CustomDate = custom(customDateDecoder, (d) => d.toISOString())

  describe('Unknown', () => {
    test('Unknown decoding', () => {
      const result = unknown.decode('no idea')
      expect(Result.isSuccess(result))

      const actual = unsafeUnwrap(result)
      expect(actual).toStrictEqual('no idea')

      type Expected = unknown
      type Actual = InferDecoded<typeof unknown>

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
      type Actual = InferDecoded<typeof boolean>

      type _Test = Expect<Equal<Expected, Actual>>
    })
  })

  describe('Number', () => {
    test('Number type inference', () => {
      expect(true).toBe(true)

      type Expected = number
      type Actual = InferDecoded<typeof number>

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
      type Actual = InferDecoded<typeof string>

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
      type Actual = InferDecoded<typeof StringArray>

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
      type Actual = InferDecoded<typeof StringArrayArray>

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
      type Actual = InferDecoded<typeof StringRecord>

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
      type Actual = InferDecoded<typeof StringRecordRecord>

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
      type Actual = InferDecoded<typeof Point>

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
      type Actual = InferDecoded<typeof NestedPoint>

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
      type Actual = InferDecodedStruct<{
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
      type Actual = InferDecoded<typeof NumOrStr>

      type _Test = Expect<Equal<Expected, Actual>>
    })

    test.each([
      [
        ['click', { location: { x: 1, y: 2 }, isDoubleClick: false }],
        ['click', { location: { x: 1, y: 2 }, isDoubleClick: false }],
      ],
      [
        ['drag', { start: { x: 0, y: 100 }, end: { x: 100, y: 100 }, duration: 300 }],
        ['drag', { start: { x: 0, y: 100 }, end: { x: 100, y: 100 }, duration: 300 }],
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
        | ['click', InferDecoded<typeof ClickEvent>]
        | ['drag', InferDecoded<typeof DragEvent>]
      type Actual = InferDecoded<typeof MouseEvent>

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

      type Actual = InferDecodedUnion<{
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
      type Actual = InferDecoded<typeof _OptionalNumber>

      type _Test = Expect<Equal<Expected, Actual>>
    })

    test('Optional type inference nested', () => {
      expect(true).toBe(true)

      // untagged unions collapse / don't nest
      const _OptionalOptionalNumber = optional(optional(number))

      type Expected = number | undefined
      type Actual = InferDecoded<typeof _OptionalOptionalNumber>

      type _Test = Expect<Equal<Expected, Actual>>
    })
  })

  describe('Custom', () => {
    test('custom decoder inference', () => {
      type Expected = Date
      type Actual = InferDecoded<typeof CustomDate>

      type _Test = Expect<Equal<Expected, Actual>>
    })

    test.each([
      ['2024-11-18', new Date(Date.UTC(2024, 10, 18)), '2024-11-18T00:00:00.000Z'],
      ['11/18/2024', new Date(Date.UTC(2024, 10, 18)), '2024-11-18T00:00:00.000Z'],
      ['11_18_2024', new Date(Date.UTC(2024, 10, 18)), '2024-11-18T00:00:00.000Z'],
    ])('custom decoder functionality', (input, decodeExpected, encodeExpected) => {
      const result: DecodeResult<Date> = CustomDate.decode(input)
      expect(Result.isSuccess(result))

      const actual: Date = unsafeUnwrap(result)
      expect(actual).toStrictEqual(decodeExpected)

      const encoded: string = CustomDate.encode(actual)
      expect(encoded).toStrictEqual(encodeExpected)
    })

    test('Custom StringMap', () => {
      type Infer1 = InferDecoded<typeof StringMap>
      type _Test1 = Expect<Equal<Map<string, string>, Infer1>>
      const result: DecodeResult<Map<string, string>> = StringMap.decode({ a: 'a', b: 'b', c: 'c' })
      expect(Result.isSuccess(result))

      const decoded: Map<string, string> = unsafeUnwrap(result)
      expect(decoded).toStrictEqual(
        new Map([
          ['a', 'a'],
          ['b', 'b'],
          ['c', 'c'],
        ]),
      )

      type Infer2 = InferEncoded<typeof StringMap>
      type _Test2 = Expect<Equal<Record<string, string>, Infer2>>
      const encoded: Record<string, string> = StringMap.encode(decoded)
      expect(encoded).toStrictEqual({ a: 'a', b: 'b', c: 'c' })
    })
  })
}
