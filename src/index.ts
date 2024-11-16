interface Shape<T> {
  readonly __tag:
    | 'unknown'
    | 'boolean'
    | 'number'
    | 'string'
    | 'bigint'
    | 'array'
    | 'record'
    | 'struct'
    | 'union'
  readonly decode: (input: unknown) => T
}

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

interface ShapeArray<S extends Shape<unknown>> extends Shape<Infer<S>[]> {
  readonly __tag: 'array'
}

// prettier-ignore
interface ShapeRecord<S extends Shape<unknown>> extends Shape<Record<string, Infer<S>>> {
  readonly __tag: 'record'
}

type StructFields = Record<string, Shape<unknown>>

// prettier-ignore
interface ShapeStruct<Fields extends StructFields> extends Shape<InferStruct<Fields>> {
  readonly __tag: 'struct'
}

type UnionVariants = Record<string, Shape<unknown>>

// prettier-ignore
interface ShapeUnion<Variants extends UnionVariants> extends Shape<InferUnion<Variants>> {
  readonly __tag: 'union'
}

export type Infer<S extends Shape<unknown>> = S extends ShapeUnknown
  ? unknown
  : S extends ShapeBoolean
    ? boolean
    : S extends ShapeNumber
      ? number
      : S extends ShapeString
        ? string
        : S extends ShapeBigInt
          ? bigint
          : S extends ShapeArray<infer ElementShape>
            ? Infer<ElementShape>[]
            : S extends ShapeRecord<infer ValueShape>
              ? Record<string, Infer<ValueShape>>
              : S extends ShapeStruct<infer Fields>
                ? InferStruct<Fields>
                : S extends ShapeUnion<infer Variants>
                  ? InferUnion<Variants>
                  : never

type InferStruct<Fields extends StructFields> = {
  [Key in keyof Fields]: Infer<Fields[Key]>
}

type InferUnion<Variants extends UnionVariants> = {
  [Key in keyof Variants]: [Key, Infer<Variants[Key]>]
}[keyof Variants]

export const unknown: ShapeUnknown = {
  __tag: 'unknown',
  decode: (input: unknown): unknown => input,
} as const

export const boolean: ShapeBoolean = {
  __tag: 'boolean',
  decode: (input: unknown): boolean => {
    if (typeof input !== 'boolean') {
      throw new Error('oopsy whoopsy!')
    }
    return input
  },
} as const

export const number: ShapeNumber = {
  __tag: 'number',
  decode(input: unknown): number {
    if (
      typeof input !== 'number' ||
      Number.isNaN(input) ||
      !Number.isFinite(input)
    ) {
      throw new Error('oopsy whoopsy!')
    }
    return input
  },
}

export const string: ShapeString = {
  __tag: 'string',
  decode(input: unknown): string {
    if (typeof input !== 'string') {
      throw new Error('oopsy whoopsy!')
    }
    return input
  },
}

export const bigint: ShapeBigInt = {
  __tag: 'bigint',
  decode(input: unknown): bigint {
    if (typeof input === 'bigint') {
      return input
    } else if (
      typeof input === 'number' &&
      !Number.isNaN(input) &&
      Number.isFinite(input)
    ) {
      return BigInt(input)
    }
    throw new Error('oopsy whoopsy!')
  },
}

function array<S extends Shape<unknown>>(elementShape: S): ShapeArray<S> {
  return {
    __tag: 'array',
    decode(input: unknown): Infer<S>[] {
      if (!Array.isArray(input)) {
        throw new Error('oopsy whoopsy!')
      }

      return input.map(elementShape.decode) as Infer<S>[]
    },
  }
}

function record<S extends Shape<unknown>>(elementShape: S): ShapeRecord<S> {
  return {
    __tag: 'record',
    decode(input: unknown): Record<string, Infer<S>> {
      if (!input || typeof input !== 'object' || Array.isArray(input)) {
        throw new Error('oopsy whoopsy!')
      }

      return Object.fromEntries(
        Object.entries(input).map(([key, value]) => [
          key,
          elementShape.decode(value) as Infer<S>,
        ]),
      )
    },
  }
}

const UnknownRecord = record(unknown)

function struct<Fields extends StructFields>(
  fieldShapes: Fields,
): ShapeStruct<Fields> {
  return {
    __tag: 'struct',
    decode(input: unknown): InferStruct<Fields> {
      const inputRecord = UnknownRecord.decode(input)

      return Object.fromEntries(
        Object.entries(fieldShapes).map(([key, shape]) => [
          key,
          shape.decode(inputRecord[key]),
        ]),
      ) as InferStruct<Fields>
    },
  }
}

function union<Variants extends UnionVariants>(
  variants: Variants,
): ShapeUnion<Variants> {
  return {
    __tag: 'union',
    decode(input: unknown): InferUnion<Variants> {
      if (!Array.isArray(input) || input.length !== 2) {
        throw new Error('oopsy whoopsy!')
      }

      const [tag, value] = input as [unknown, unknown]

      if (typeof tag !== 'string' || !Object.keys(variants).includes(tag)) {
        throw new Error('oopsy whoopsy!')
      }

      return [tag, variants[tag].decode(value) as Infer<Variants[typeof tag]>]
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

  describe('Unknown', () => {
    test('Unknown decoding', () => {
      expect(unknown.decode('no idea')).toStrictEqual('no idea')

      type Expected = unknown
      type _Test = Expect<Equal<Infer<typeof unknown>, Expected>>
    })
  })

  describe('Boolean', () => {
    test('Boolean decoding', () => {
      expect(boolean.decode(true)).toStrictEqual(true)
      expect(boolean.decode(false)).toStrictEqual(false)

      type Expected = boolean
      type _Test = Expect<Equal<Infer<typeof boolean>, Expected>>
    })
  })

  describe('Number', () => {
    test('Number decoding', () => {
      expect(number.decode(16)).toStrictEqual(16)
      expect(number.decode(0)).toStrictEqual(0)
      expect(number.decode(-0)).toStrictEqual(-0)
      expect(number.decode(-16)).toStrictEqual(-16)

      type Expected = number
      type _Test = Expect<Equal<Infer<typeof number>, Expected>>
    })
  })

  describe('String', () => {
    test('String decoding', () => {
      expect(string.decode('')).toStrictEqual('')
      expect(string.decode('hello')).toStrictEqual('hello')

      type Expected = string
      type _Test = Expect<Equal<Infer<typeof string>, Expected>>
    })
  })

  describe('Array', () => {
    test('Array decoding', () => {
      const StringArray = array(string)

      expect(StringArray.decode(['a', 'b', 'c'])).toStrictEqual(['a', 'b', 'c'])

      type Expected = string[]
      type _Test = Expect<Equal<Infer<typeof StringArray>, Expected>>
    })

    test('nested Array decoding', () => {
      const StringArrayArray = array(array(string))

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
      const StringRecord = record(string)

      expect(StringRecord.decode({ a: 'aa', b: 'bb' })).toStrictEqual({
        a: 'aa',
        b: 'bb',
      })

      type Expected = Record<string, string>
      type Actual = Infer<typeof StringRecord>

      type _Test = Expect<Equal<Actual, Expected>>
    })

    test('nested Record decoding', () => {
      const StringRecordRecord = record(record(string))

      expect(
        StringRecordRecord.decode({ a: { aa: 'aaa' }, b: { bb: 'bbb' } }),
      ).toStrictEqual({ a: { aa: 'aaa' }, b: { bb: 'bbb' } })

      type Expected = Record<string, Record<string, string>>
      type Actual = Infer<typeof StringRecordRecord>

      type _Test = Expect<Equal<Actual, Expected>>
    })
  })

  describe('Struct', () => {
    test('Struct decoding', () => {
      const Point = struct({
        x: number,
        y: number,
      })

      expect(Point.decode({ x: 1, y: 2 })).toStrictEqual({ x: 1, y: 2 })

      type Expected = {
        x: number
        y: number
      }
      type Actual = Infer<typeof Point>

      type _Test = Expect<Equal<Actual, Expected>>
    })

    test('nested Struct decoding', () => {
      const Point = struct({
        x: number,
        y: number,
      })
      const NestedPoint = struct({
        start: Point,
        end: Point,
      })

      expect(
        NestedPoint.decode({ start: { x: 1, y: 2 }, end: { x: 10, y: 20 } }),
      ).toStrictEqual({ start: { x: 1, y: 2 }, end: { x: 10, y: 20 } })

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

      type _Test = Expect<Equal<Actual, Expected>>
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

      type _Test = Expect<Equal<Actual, Expected>>
    })
  })

  describe('Union', () => {
    test('Union decoding', () => {
      const NumOrStr = union({
        num: number,
        str: string,
      })

      expect(NumOrStr.decode(['num', 16])).toStrictEqual(['num', 16])
      expect(NumOrStr.decode(['str', 'hello'])).toStrictEqual(['str', 'hello'])

      type Expected = ['num', number] | ['str', string]
      type Actual = Infer<typeof NumOrStr>

      type _Test = Expect<Equal<Actual, Expected>>
    })

    test('nested Union decoding', () => {
      const Point = struct({
        x: number,
        y: number,
      })
      const ClickEvent = struct({
        point: Point,
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

      expect(
        MouseEvent.decode([
          'click',
          { isDoubleClick: false, point: { x: 100, y: 200 } },
        ]),
      ).toStrictEqual([
        'click',
        { isDoubleClick: false, point: { x: 100, y: 200 } },
      ])

      type Expected =
        | ['click', Infer<typeof ClickEvent>]
        | ['drag', Infer<typeof DragEvent>]
      type Actual = Infer<typeof MouseEvent>

      type _Test = Expect<Equal<Actual, Expected>>
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

      type _Test = Expect<Equal<Actual, Expected>>
    })
  })
}
