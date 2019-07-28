import faker from 'faker'
import nanoid from 'nanoid'
import times from 'ramda/es/times'

function createFakeProject({ sortIdx }) {
  const now = Date.now()

  return {
    id: nanoid(),
    title:
      faker.hacker.verb() +
      ' ' +
      faker.hacker.ingverb() +
      ' ' +
      faker.hacker.noun(),
    sortIdx,
    createdAt: now,
    modifiedAt: now,
  }
}

export function createFakeProjects() {
  return times(i => createFakeProject({ sortIdx: i }))(5)
}

// const fakePrjs = createFakeProjects()
// fakePrj.forEach(p => console.log(p))
// localStorage.setItem('projectList', JSON.stringify(fakePrjs))
