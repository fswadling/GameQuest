using Microsoft.FSharp.Core;
using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Graphics;
using Microsoft.Xna.Framework.Input;
using Myra;
using static Utilities;

namespace GameQuest
{
    public class Game1 : Game
    {
        private GraphicsDeviceManager _graphics;
        private SpriteBatch _spriteBatch;
        private StaticCoordinationRunner staticCoordinationRunner;
        private FSharpOption<IMenu> menu;

        public Game1()
        {
            _graphics = new GraphicsDeviceManager(this);
            Content.RootDirectory = "Content";
            IsMouseVisible = true;
        }

        protected override void Initialize()
        {
            base.Initialize();
        }

        private void OnMenuJourneyEvent(Utilities.MenuJourneyEvent e)
        {
            this.menu = this.staticCoordinationRunner.DoStep(e);

            if (this.menu != null)
            {
                this.menu.Value.Initialise();
            }
        }

        protected override void LoadContent()
        {
            _spriteBatch = new SpriteBatch(GraphicsDevice);

            MyraEnvironment.Game = this;

            this.staticCoordinationRunner = new StaticCoordinationRunner(MenuJourney.StartMenuJouney(this.OnMenuJourneyEvent));

            this.menu = this.staticCoordinationRunner.DoStep(MenuJourneyEvent.Initialise);

            if (this.menu != null)
            {
                this.menu.Value.Initialise();
            }
        }

        protected override void Update(GameTime gameTime)
        {
            if (GamePad.GetState(PlayerIndex.One).Buttons.Back == ButtonState.Pressed || Keyboard.GetState().IsKeyDown(Keys.Escape))
                Exit();

            if (this.menu != null)
            {
                this.menu.Value.OnUpdate(gameTime);
            }

            base.Update(gameTime);
        }

        protected override void Draw(GameTime gameTime)
        {
            GraphicsDevice.Clear(Color.Black);

            if (this.menu != null)
            {
                this.menu.Value.OnRender();
            }

            base.Draw(gameTime);
        }
    }
}