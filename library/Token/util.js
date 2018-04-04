function tokenizeTrack(track, langDef, sDef) {
  const stateStore = [[]]
  const states = ['root']
  const sfStates = [0]
  const length = track.length
  let depth = 0
  let pointer = 0
  while (pointer < length) {
    const temp = track.slice(pointer)
    const slice = temp.trimLeft()
    if (slice === '') break
    pointer += temp.length - slice.length
    let matched = false
    const patterns = langDef[states[depth]]
    const patternLength = patterns.length

    for (let index = 0; index < patternLength; index++) {
      const element = patterns[index]
      const match = slice.match(element.regex)
      if (match === null) continue
      matched = true
      const action = 'cases' in element.action ? match[0] in element.action.cases ? element.action.cases[match[0]] : element.action.cases['@default'] : element.action
      if (action.token === 'usfunc' || action.token === 'undef') {
        sfStates[depth] += 1
      }
      if ('next' in action) {
        if (action.token !== '@pass') {
          stateStore[depth].push(((p) => (content) => Object.assign(action.transform(match, content, (track) => tokenizeTrack(track, langDef, sDef)), { StartIndex: p }))(pointer))
        }
        if (action.next === '@pop') {
          depth -= 1
          states.pop()
          const state = stateStore.pop()
          mergeSimplifiedFunc(state, sfStates.pop(), langDef, sDef)
          stateStore[depth].push(stateStore[depth].pop()(state))
        } else {
          stateStore.push([])
          states.push(action.next)
          sfStates.push(0)
          depth += 1
        }
      } else if (action.token !== '@pass') {
        stateStore[depth].push(Object.assign(action.transform(match, (track) => tokenizeTrack(track, langDef, sDef)), { StartIndex: pointer }))
      }
      pointer += match[0].length
      break
    }
    if (!matched) {
      throw new Error() // Temporarily added to debug TODO: remove in the future
      // stateStore.push(track.charAt(pointer))
      // pointer += 1
    }
  }
  const state = stateStore[0]
  mergeSimplifiedFunc(state, sfStates.pop(), langDef, sDef)
  return state
}

function mergeSimplifiedFunc(state, count, langDef, sDef) {
  if (count === 0) return
  let lastCount = -1
  while (lastCount !== count) {
    lastCount = count
    for (let i = 0; i < sDef.length; i++) {
      const pattern = sDef[i]
      const patternLength = pattern.pat.length
      for (let j = 0; j <= state.length - patternLength; j++) {
        const status = compare(pattern.pat, patternLength, state, j)
        if (status !== -1) {
          state.splice(j, patternLength, pattern.transform(state.slice(j, j + patternLength), (track) => tokenizeTrack(track, langDef, sDef)))
          count -= status
          if (count === 0) return
        }
      }
    }
  }
}

function compare(pattern, patternLength, state, startIndex) {
  let count = 0
  for (let k = 0; k < patternLength; k++) {
    const part = pattern[k]
    const ori = state[startIndex + k]
    if (!sameType(part, ori)) return -1
    switch (part.Type) {
      case 'Sfunc':
        for (let l = 0, length = part.Content.length; l < length; l++) {
          if (part.Content[l] instanceof RegExp) {
            if (ori.Content[l].Type !== 'Dyn' || !part.Content[l].test(ori.Content[l].Content)) {
              return -1
            }
          } else if (part.Content[l].Type !== ori.Content[l].Type) {
            return -1
          }
        }
        count += 1
        break
      case 'Undef':
        if (part.Content !== ori.Content) {
          return -1
        }
        count += 1
    }
  }
  return count
}

function sameType(pat, sta) {
  return typeof pat.Type === 'string' ? pat.Type === sta.Type : pat.Type.includes(sta.Type)
}

module.exports = {
  tokenizeTrack,
  mergeSimplifiedFunc,
  compare,
  sameType
}
