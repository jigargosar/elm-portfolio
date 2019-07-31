import { mergeRight } from 'ramda'

const nanoid = require('nanoid')
const faker = require('faker')
const logger = require('koa-logger')
const router = require('koa-router')()
const koaBody = require('koa-body')

const Koa = require('koa')
const app = module.exports = new Koa()


// middleware

app.use(logger())


app.use(koaBody())

// route definitions

router.get('/', hello)
router.get('/hello', hello)
router.get('/all', async ctx => {
  ctx.body = { todoList: [] }
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
