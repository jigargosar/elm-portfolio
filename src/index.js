import forEachObjIndexed from 'ramda/es/forEachObjIndexed'
import fromPairs from 'ramda/es/fromPairs'
import path from 'ramda/es/path'
import values from 'ramda/es/values'
import './index.css'
// @ts-ignore
import { Elm } from './Main.elm'

const app = Elm.Main.init({
  flags: {
    modelCache: JSON.parse(localStorage.getItem('modelCache')||"null")
  },
})

const subs = {
  cacheKeyValue :([k, v])=>{
    console.log("cacheKeyValue", [k,v])
    localStorage.setItem(k,JSON.stringify(v))
  }
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
