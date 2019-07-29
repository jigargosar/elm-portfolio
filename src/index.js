import fromPairs from 'ramda/es/fromPairs'
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
  },
})

if (app.ports) {
  app.ports.cacheTodoList &&
    app.ports.cacheTodoList.subscribe(list => {
      const taskMap = fromPairs(list.map(t => [t.id, t]))
      console.debug('persistingTaskMap', taskMap)
      localStorage.setItem('taskMap', JSON.stringify(taskMap))
    })

  app.ports.cacheEdit &&
    app.ports.cacheEdit.subscribe(edit => {
      console.log('app.ports.cacheEdit', edit)
      localStorage.setItem('edit', JSON.stringify(edit))
    })
}
