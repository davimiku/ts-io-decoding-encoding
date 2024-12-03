type ResultSuccess<T> = { tag: 'success'; value: T }
type ResultError<Err> = { tag: 'error'; error: Err }
export type Result<T, Err> = ResultSuccess<T> | ResultError<Err>

/**
 * Creates a `Result` of the "success" variant with the given value
 */
function success<T>(value: T): ResultSuccess<T> {
  return { tag: 'success', value }
}

/**
 * Checks if the given `Result` is of the "success" variant, narrowing if so
 */
function isSuccess<T, Err>(result: Result<T, Err>): result is ResultSuccess<T> {
  return result.tag === 'success'
}

/**
 * Creates a `Result` of the "failure" variant with the given `Error`
 */
function error<Err>(error: Err): ResultError<Err> {
  return { tag: 'error', error }
}

/**
 * Checks if the given `Result` is of the "error" variant, narrowing if so
 */
function isError<T, Err>(result: Result<T, Err>): result is ResultError<Err> {
  return result.tag === 'error'
}

/**
 * Maps the value of the `Result` with the given mapper if it is
 * the "success" variant, otherwise returns the same `Result`
 */
function map<T, U, Err>(
  result: Result<T, Err>,
  mapper: (value: T) => U,
): Result<U, Err> {
  if (isSuccess(result)) {
    return success(mapper(result.value))
  }
  return result
}

/**
 * Maps the error of the `Result` with the given mapper if it is
 * the "error" variant, otherwise returns the same `Result`
 */
function mapError<T, Err1, Err2>(
  result: Result<T, Err1>,
  mapper: (err: Err1) => Err2,
): Result<T, Err2> {
  if (isError(result)) {
    return error(mapper(result.error))
  }
  return result
}

/**
 * Flatmaps the value of the `Result` with the given mapper if it is
 * the "success" variant, otherwise returns the same `Result`
 */
function flatMap<T, U, Err>(
  result: Result<T, Err>,
  mapper: (value: T) => Result<U, Err>,
): Result<U, Err> {
  if (isSuccess(result)) {
    return mapper(result.value)
  }
  return result
}

export const Result = {
  success,
  isSuccess,
  error,
  isError,
  map,
  mapError,
  flatMap,
} as const
