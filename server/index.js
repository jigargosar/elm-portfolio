const { fromPairs, values } = require('ramda')

const { mergeRight, times } = require('ramda')
const nanoid = require('nanoid')
const faker = require('faker')
const Conf = require('conf')

const logger = require('koa-logger')
const router = require('koa-router')()
const koaBody = require('koa-body')

const Koa = require('koa')
const app = module.exports = new Koa()

const config = new Conf({
  cwd: process.cwd(),
  clearInvalidConfig: false,
  configName: 'fake-db',
  defaults: {
    db: createFakeDB(),
  },
})


// console.log(config.get('db'))

// middleware

app.use(logger())


app.use(koaBody())

// route definitions

router.get('/', hello)
router.get('/hello', hello)
router.get('/db', async ctx => {
  ctx.body = config.get('db')
})
router.post('/db', ctx => {
    const reqBodyString = ctx.request.body
    const db = JSON.parse(reqBodyString)
    console.log('parsed body', db)

    config.set('db.projectList', db.projectList)
    config.set('db.todoList', db.todoList)
    ctx.body = db
  },
)

function getTodoDict() {
  const todoList = config.get('db.todoList')
  const todoDict = fromPairs(todoList.map(t => [t.id, t]))
  return todoDict
}

function setTodoDict(todoDict) {
  config.set('db.todoList', values(todoDict))
}

router.post('/sync', ctx => {
    const patchList = ctx.request.body

    console.log('parsed body patchList', patchList)

    const todoDict = getTodoDict()

    console.log(values(todoDict))
    patchList.forEach(p => {

      const todo = todoDict[p.todoId]
      todo[p.key] = p.value
      todo.modifiedAt = p.modifiedAt

    })

    console.log(values(todoDict))

    setTodoDict(todoDict)

    ctx.body = config.get('db')
  },
)
// .get('/post/new', add)
// .get('/post/:id', show)
// .post('/post', create);

app.use(router.routes())


async function hello(ctx) {
  // await ctx.render('list', { posts: posts })

  ctx.body = { msg: 'ECHO', payload: 'payload10' }
}

if (!module.parent) app.listen(3000)


function createFakeTask(overrides) {
  const now = Date.now()
  return mergeRight({
    id: nanoid(),
    title: faker.hacker.phrase(),
    sortIdx: 0,
    projectId: '',
    isDone: faker.random.boolean(),
    createdAt: now,
    modifiedAt: now,
  }, overrides)
}

function createFakeProject(overrides) {
  const now = Date.now()

  return mergeRight({
    id: nanoid(),
    title:
      faker.hacker.verb() +
      ' ' +
      faker.hacker.ingverb() +
      ' ' +
      faker.hacker.noun(),
    sortIdx: 0,
    createdAt: now,
    modifiedAt: now,
  }, overrides)
}

function createFakeDB() {
  faker.seed(12333)
  const prjFromIdx = sortIdx => createFakeProject({ sortIdx })
  const projectList = times(prjFromIdx)(5)
  const todoFromIdx = sortIdx => createFakeTask({ sortIdx })
  const todoList = times(todoFromIdx)(20)
  return {
    projectList,
    todoList,
  }
}
