import forEachObjIndexed from 'ramda/es/forEachObjIndexed'
import fromPairs from 'ramda/es/fromPairs'
import path from 'ramda/es/path'
import values from 'ramda/es/values'
import './index.css'
// @ts-ignore
import { Elm } from './Main.elm'

function createAndAppendRoot() {
  const root = document.createElement('div')
  root.id = 'root'
  document.body.appendChild(root)
  return root
}

const app = Elm.Main.init({
  node: document.getElementById('root') || createAndAppendRoot(),
  flags: {
    todoList: values(JSON.parse(localStorage.getItem('taskMap') || '{}')),
    // todoList: [{}],
    projectList: JSON.parse(localStorage.getItem('projectList') || '[]'),
    edit: JSON.parse(localStorage.getItem('edit')),
  },
})

const subs = {
  cacheTodoList: list => {
    const taskMap = fromPairs(list.map(t => [t.id, t]))
    console.debug('persistingTaskMap', taskMap)
    localStorage.setItem('taskMap', JSON.stringify(taskMap))
  },
  cacheEdit: edit => {
    console.log('app.ports.cacheEdit', edit)
    localStorage.setItem('edit', JSON.stringify(edit))
  },
}

forEachObjIndexed((listener, portName) => {
  const subscribe = path(['ports', portName, 'subscribe'])(app)
  if (!subscribe) {
    console.warn('Subscription Port Not Found:', portName, app)
    return
  }
  console.log('Subscription Port Attached', portName)
  subscribe(listener)
})(subs)
