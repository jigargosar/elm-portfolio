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
  configName:'fake-db',
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
router.get(['/all', '/db'], async ctx => {
  ctx.body = config.get('db')
})
// .get('/post/new', add)
// .get('/post/:id', show)
// .post('/post', create);

app.use(router.routes())


async function hello(ctx) {
  // await ctx.render('list', { posts: posts })
  ctx.body = { msg: 'ECHO', payload: 'payload10' }
}


// async function add(ctx) {
//   await ctx.render('new')
// }
//
//
// async function show(ctx) {
//   const id = ctx.params.id
//   const post = posts[id]
//   if (!post) ctx.throw(404, 'invalid post id')
//   await ctx.render('show', { post: post })
// }
//
//
// async function create(ctx) {
//   const post = ctx.request.body
//   const id = posts.push(post) - 1
//   post.created_at = new Date()
//   post.id = id
//   ctx.redirect('/')
// }


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