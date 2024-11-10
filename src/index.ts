type ShapeType =
  | 'unknown'
  | 'boolean'
  | 'number'
  | 'string'
  | 'array'
  | 'record'
  | 'struct'
  | 'union'

interface Shape<T = unknown> {
  readonly __type: ShapeType
  readonly decode: (input: unknown) => T
}

interface ShapeUnknown extends Shape {
  readonly __type: 'unknown'
  readonly decode: (input: unknown) => unknown
}

interface ShapeBoolean extends Shape {
  readonly __type: 'boolean'
  readonly decode: (input: unknown) => boolean
}

interface ShapeString extends Shape {
  readonly __type: 'string'
  readonly decode: (input: unknown) => string
}

interface ShapeNumber extends Shape {
  __type: 'number'
  decode(input: unknown): number
}

interface ShapeArray<T> extends Shape {
  __type: 'array'
  // elementShape: S
  decode(input: unknown): T[]
}

interface ShapeRecord<T> extends Shape {
  __type: 'record'
  decode(input: unknown): Record<string, T>
}

interface ShapeStruct<Fields extends Record<string, Shape>> extends Shape {
  __type: 'struct'
  decode(input: unknown): InferStruct<Fields>
}

interface ShapeUnion<
  TagName extends string,
  Variants extends Record<string, Shape>,
> extends Shape {
  __type: 'union'
  decode(input: unknown): InferUnion<TagName, Variants>
}

export type Infer<S extends Shape> = S extends ShapeUnknown
  ? unknown
  : S extends ShapeBoolean
    ? boolean
    : S extends ShapeString
      ? string
      : S extends ShapeNumber
        ? number
        : S extends ShapeArray<infer Element>
          ? Element[]
          : S extends ShapeRecord<infer Value>
            ? Record<string, Value>
            : S extends ShapeStruct<infer Fields>
              ? InferStruct<Fields>
              : S extends ShapeUnion<infer TagName, infer Variants>
                ? InferUnion<TagName, Variants>
                : never

type InferStruct<Fields extends Record<string, Shape>> = {
  [Key in keyof Fields]: Infer<Fields[Key]>
}

type InferUnion<
  TagName extends string,
  Variants extends Record<string, Shape>,
> = {}

function unknown(): ShapeUnknown {
  return {
    __type: 'unknown',
    decode: (input: unknown): unknown => input,
  }
}

function boolean(): ShapeBoolean {
  return {
    __type: 'boolean',
    decode: (input: unknown): boolean => typeof input === 'boolean' && input,
  }
}

function string(): ShapeString {
  return {
    __type: 'string',
    decode(input: unknown): string {
      if (typeof input !== 'string') {
        throw new Error('oops!')
      }
      return input
    },
  }
}

function number(): ShapeNumber {
  return {
    __type: 'number',
    decode(input: unknown): number {
      if (
        typeof input !== 'number' ||
        Number.isNaN(input) ||
        !Number.isFinite(input)
      ) {
        throw new Error('oops!')
      }
      return input
    },
  }
}

function array<S extends Shape, El = Infer<S>>(
  elementShape: Shape<El>,
): ShapeArray<El> {
  return {
    __type: 'array',
    decode(input: unknown): El[] {
      if (!Array.isArray(input)) {
        throw new Error('oops!')
      }

      return input.map(elementShape.decode)
    },
  }
}

type GetElementType<Arr extends unknown[]> =
  Arr extends Array<infer T> ? T : never
type Test1111 = GetElementType<string[]>

function record<S extends Shape, El = Infer<S>>(
  elementShape: Shape<El>,
): ShapeRecord<El> {
  return {
    __type: 'record',
    decode(input: unknown): Record<string, El> {
      if (!input || typeof input !== 'object' || Array.isArray(input)) {
        throw new Error('oops!')
      }

      return Object.fromEntries(
        Object.entries(input).map(([key, value]) => [
          key,
          elementShape.decode(value),
        ]),
      )
    },
  }
}

const UnknownRecord = record(unknown())

function struct<Fields extends Record<string, Shape>>(
  fieldShapes: Fields,
): ShapeStruct<Fields> {
  return {
    __type: 'struct',
    decode(input: unknown): InferStruct<Fields> {
      const record = UnknownRecord.decode(input)

      // The generic types don't survive through Object.entries, so a type
      // assertion keeps us on track here
      // eslint-disable-next-line @typescript-eslint/consistent-type-assertions
      return Object.fromEntries(
        Object.entries(fieldShapes).map(([key, shape]) => [
          key,
          shape.decode(record[key]),
        ]),
      ) as InferStruct<Fields>
    },
  }
}

function union() {}

export const IO = {
  unknown: unknown(),
  boolean: boolean(),
  string: string(),
  number: number(),
  array,
  record,
  struct,
} as const

if (import.meta.vitest) {
  const { test, expect, describe } = import.meta.vitest

  // https://www.totaltypescript.com/how-to-test-your-types
  type Expect<T extends true> = T
  // prettier-ignore
  type Equal<X, Y> = (<T>() => T extends X ? 1 : 2) extends (<T>() => T extends Y ? 1 : 2)
      ? true
      : false

  describe('Unknown', () => {
    test('Unknown decoding', () => {
      expect(IO.unknown.decode('no idea')).toStrictEqual('no idea')

      type Expected = unknown
      type _Test = Expect<Equal<Infer<typeof IO.unknown>, Expected>>
    })
  })

  describe('Boolean', () => {
    test('Boolean decoding', () => {
      expect(IO.boolean.decode(true)).toStrictEqual(true)
      expect(IO.boolean.decode(false)).toStrictEqual(false)

      type Expected = boolean
      type _Test = Expect<Equal<Infer<typeof IO.boolean>, Expected>>
    })
  })

  describe('Number', () => {
    test('Number decoding', () => {
      expect(IO.number.decode(42)).toStrictEqual(42)
      expect(IO.number.decode(0)).toStrictEqual(0)
      expect(IO.number.decode(-0)).toStrictEqual(-0)

      expect(IO.number.decode(-42)).toStrictEqual(-42)
      type Expected = number
      type _Test = Expect<Equal<Infer<typeof IO.number>, Expected>>
    })
  })

  describe('String', () => {
    test('String decoding', () => {
      expect(IO.string.decode('')).toStrictEqual('')
      expect(IO.string.decode('hello')).toStrictEqual('hello')

      type Expected = string
      type _Test = Expect<Equal<Infer<typeof IO.string>, Expected>>
    })
  })

  describe('Array', () => {
    test('Array decoding', () => {
      const StringArray = IO.array(IO.string)

      expect(StringArray.decode(['a', 'b', 'c'])).toStrictEqual(['a', 'b', 'c'])

      type Expected = string[]
      type _Test = Expect<Equal<Infer<typeof StringArray>, Expected>>
    })

    test('nested Array decoding', () => {
      const StringArrayArray = IO.array(IO.array(IO.string))

      expect(
        StringArrayArray.decode([
          ['a', 'b'],
          ['c', 'd'],
          ['e', 'f'],
        ]),
      ).toStrictEqual([
        ['a', 'b'],
        ['c', 'd'],
        ['e', 'f'],
      ])

      type Expected = string[][]
      type _Test = Expect<Equal<Infer<typeof StringArrayArray>, Expected>>
    })
  })

  describe('Record', () => {
    test('Record decoding', () => {
      const StringRecord = IO.record(IO.string)

      expect(StringRecord.decode({ a: 'aa', b: 'bb' })).toStrictEqual({
        a: 'aa',
        b: 'bb',
      })

      type Expected = Record<string, string>
      type _Test = Expect<Equal<Infer<typeof StringRecord>, Expected>>
    })

    test('nested Record decoding', () => {
      const StringRecordRecord = IO.record(IO.record(IO.string))

      expect(
        StringRecordRecord.decode({ a: { aa: 'aaa' }, b: { bb: 'bbb' } }),
      ).toStrictEqual({ a: { aa: 'aaa' }, b: { bb: 'bbb' } })

      type Expected = Record<string, string>
      type _Test = Expect<
        Equal<Infer<typeof StringRecordRecord>, Record<string, Expected>>
      >
    })
  })

  describe('Struct', () => {
    test('Struct decoding', () => {
      const Point = IO.struct({
        x: IO.number,
        y: IO.number,
      })

      expect(Point.decode({ x: 1, y: 2 })).toStrictEqual({ x: 1, y: 2 })

      type Expected = {
        x: number
        y: number
      }
      type _Test = Expect<Equal<Infer<typeof Point>, Expected>>
    })

    test('nested Struct decoding', () => {
      const Point = IO.struct({
        x: IO.number,
        y: IO.number,
      })
      const NestedPoint = IO.struct({
        start: Point,
        end: Point,
      })

      expect(
        NestedPoint.decode({ start: { x: 1, y: 2 }, end: { x: 10, y: 20 } }),
      ).toStrictEqual({ start: { x: 1, y: 2 }, end: { x: 10, y: 20 } })

      type _Test = Expect<
        Equal<
          Infer<typeof NestedPoint>,
          { start: { x: number; y: number }; end: { x: number; y: number } }
        >
      >
    })

    test('Struct type inference', () => {
      expect(true).toBe(true);

      type _Test111 = InferStruct<{x: ShapeNumber, s: ShapeString, a: ShapeArray<ShapeBoolean> }>
    })
  })
}
