const fs = require("fs")
const deserializer = require("../Deserializer.bs.js")
const vm = require("../AmbientReducer.bs.js")

const IPFS = require('ipfs')
const OrbitDB = require('orbit-db')
const IdentityProvider = require('orbit-db-identity-provider')

const jsonFile = "./__tests__/fixtures/002.json"
const json = fs.readFileSync(jsonFile, "utf8")
const program = deserializer.fromJSON(json)

const ipfs = new IPFS({
  start: false,
  repo: './ipfs'
})

ipfs.on('ready', async () => {
  try {
    const orbitdb = await OrbitDB.createInstance(ipfs, {offline: true})
    const db = await orbitdb.log('ambients-reducer')

    let events = []

    const onEvent = async ([s, e, t]) => {
      events.push({ source: s, event: e, target: t })
      console.log(">> event:", s, e, t)
    }

    console.log(">> Reducing program...")
    const st1 = new Date().getTime()
    // const result = await vm.reduceFully(onEvent, program)
    // const result = await vm.reduceFullyDebug(0, onEvent, program)
    const result = await vm.reduceToValue(onEvent, program)
    const et1 = new Date().getTime()
    console.log(">> fully reduced! (" + (et1-st1) + " ms)")
    console.log(result[0])

    for (e of events) {
      await db.add(e)
      console.log(">> write event:", e.source, e.event, e.target)
    }

    const et2 = new Date().getTime()
    console.log(">> log fully written! (" + (et2-st1) + " ms)")
    
    const values = db.iterator({limit: -1}).collect()
    // console.log(values.map(e => JSON.stringify(e.payload.value)))
  } catch (e) {
    console.error(e)
    process.exit(1)
  }

  process.exit(0)
})
